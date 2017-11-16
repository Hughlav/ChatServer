
{-# LANGUAGE LambdaCase,RecordWildCards, OverloadedStrings #-} 
module Main where
  
  
  import System.IO
  import Control.Exception
  --import Control.Concurrent
  import Control.Monad (forever,replicateM,when,join)
  import Control.Monad.Fix (fix)
  import Control.Applicative
  import Control.Concurrent (forkFinally)
  import Control.Concurrent.STM
  import Control.Concurrent.Async
  import Text.Printf (hPrintf, printf)
  import Data.Map (Map)
  import qualified Data.Map as M
  import Data.Set (Set)
  import Network
  import Network (PortID(..), accept, listenOn, withSocketsDo)
  import qualified Data.Set as S
  import Data.Hashable


  
  {- PORT 4242
  To join a chat client sends
        JOIN_CHATROOM: [chatroom name]
        CLIENT_IP: [IP Address of client if UDP | 0 if TCP]
        PORT: [port number of client if UDP | 0 if TCP]
        CLIENT_NAME: [string Handle to identifier client user]
  
  Server responds with
        JOINED_CHATROOM: [chatroom name]
        SERVER_IP: [IP address of chat room]
        PORT: [port number of chat room]
        ROOM_REF: [integer that uniquely identifies chat room on server]
        JOIN_ID: [integer that uniquely identifies client joining]
            
  Server should also let chat room know that "CLIENT_NAME" joint chat room
  
  To leave a chatroom client sends 
        LEAVE_CHATROOM: [ROOM_REF]
        JOIN_ID: [integer previously provided by server on join]
        CLIENT_NAME: [string Handle to identifier client user]
            
  The server responds with the following message:
        LEFT_CHATROOM: [ROOM_REF]
        JOIN_ID: [integer previously provided by server on join]
  
  Sever should also let chat room know "CLIENT_NAME" left
  
  To terminate connection Client sends:
        DISCONNECT: [IP address of client if UDP | 0 if TCP]
        PORT: [port number of client it UDP | 0 id TCP]
        CLIENT_NAME: [string handle to identify client user]
            
  To send a message Client sends:
        CHAT: [ROOM_REF]
        JOIN_ID: [integer identifying client to server]
        CLIENT_NAME: [string identifying client user]
        MESSAGE: [string terminated with '\n\n']
            
  Server sents chat group:
        CHAT: [ROOM_REF]
        CLIENT_NAME: [string identifying client user]
            MESSAGE: [string terminated with '\n\n']
            
  Error message 
        ERROR_CODE: [integer]
        ERROR_DESCRIPTION: [string describing error]
  
  -}

  portNum :: Int 
  portNum = 2888
  
  type ClientName = String
  type RoomName = String

  killSERV :: String
  killSERV = "KILL"
  
--check
  --type ErrorHeading = String
  --type ErrorBody = String
  --type CmdArgs = [[String]]

  data Message = Notice String
      | Tell String -- ClientName
      | Broadcast String -- ClientName
      | Command [[String]] String --CmdArgs
      | Error String String -- ErrorHeading
      deriving Show



  data Client = Client
      { clientName :: ClientName
      , clientChan :: TChan Message
      , clientHandle :: Handle 
      , clientID :: Int
  }
  
  data Room = Room
      { roomName :: RoomName
      , roomID :: Int
      , clients :: TVar (Map Int Client) -- Store list of clients in room
  }
  
  --type Msg = (Int, String)
  type RoomList = TVar (Map Int Room) -- Store list of rooms
  type Server = TVar (Map Int Room)


  main :: IO()
  main = withSocketsDo $ do
      server <- newServer
      printf "waiting for connction"
      sock <- listenOn (PortNumber (fromIntegral portNum))
      
      forever $ do 
            (handle, host, port) <- accept sock
            printf "Connection accepted: %s\n" host
            forkFinally (handleClient handle server) (\_ -> hClose handle) --fork each client to its own thread
      
  
  newServer :: IO Server
  newServer = newTVarIO M.empty


--do i need ID
  createClient :: ClientName -> Handle -> Int -> IO Client
  createClient name handle nameHash = do
      chan <- newTChanIO
      printf "Creating Client\n"
      return Client { clientName = name
                    , clientChan = chan
                    , clientHandle = handle
                    , clientID = nameHash
                    }

  sendMsg :: Client -> Message -> STM ()
  sendMsg Client{..} msg = writeTChan clientChan msg -- {..} pattern matching so clientChan is easily accesible 
      
  sendMsgtoRoom :: Message -> Room -> IO ()
  sendMsgtoRoom msg room@Room{..} = atomically $ do 
      clientList <- readTVar clients --return a list of clients in a room from clients TVar
      let roomClients = M.elems clientList --access properties of clients
      mapM_ (\a -> sendMsg a msg) roomClients --send the message to each member in the room
  
  handleMsg :: Server -> Client -> Message -> IO Bool
  handleMsg serv client@Client{..} msg = 
      case msg of --------------- STUCK IN HANDLEMSG
            Notice message -> output message
            Tell message -> output message
            Broadcast message -> output message
            Error head message -> output $ "->" ++ head ++ "<-\n" ++ message
            Command message arg -> case message of
                  [["CLIENT_IP:",_],["PORT:",_],["CLIENT_NAME:",name]] -> do
                        putStrLn "client joined chatroom\n"
                        joinChatRoom client serv arg 
                        let joinmsg = "CHAT:" ++(show (hash arg))++"\nCLIENT_NAME:" ++ clientName ++ "\n has joined the chatroom.\n"
                        tellRoom (read arg :: Int) (Tell joinmsg) --tell/broadcast
                        putStrLn "Room notified. returning True.\n"
                        return True
                  [["JOIN_ID:",id],["CLIENT_NAME:",name]] -> do
                        putStrLn "leave chatroom\n"
                        leaveChatroom client serv (read arg :: Int)
                        return True 
                  [["PORT:",_],["CLIENT_NAME:",name]] -> do
                        putStrLn "dissconnect\n"
                        removeClient serv client
                        return False
                  [["JOIN_ID:",id],["CLIENT_NAME:",name],("MESSAGE:":msgToSend),[]] -> do
                        putStrLn "send msg\n"
                        tellRoom (read arg :: Int) $ Tell ("CHAT:" ++ arg ++ "\nCLIENT_NAME: " ++ name ++ "\nMESSAGE: "++(unwords msgToSend)++"\n\n")
                        return True
                  [["KILL"]] -> do
                        putStrLn "KILL\n"
                        if arg == killSERV then return False
                        else return True
                  _ -> do
                        printf "Error\n"
                        atomically   $ sendMsg client $ Error "Error " "Unrecognised args"
                        return True
                  where 
                        tellRoom roomID msg = do
                              roomsList <- atomically $ readTVar serv
                              let maybeRoom = M.lookup roomID roomsList
                              case maybeRoom of
                                Nothing    -> return True
                                Just a -> sendMsgtoRoom msg a >> return True
            where output s = do putStrLn (clientName ++ "msg = " ++ s) >> hPutStrLn clientHandle s; return True

  
  handleClient :: Handle -> Server -> IO()
  handleClient handle server = do
      printf "handling client\n"
      hSetNewlineMode handle universalNewlineMode
      hSetBuffering handle NoBuffering
      readNxt
      return ()
      where
            readNxt = do --stck here i think check newlines etc
                  nxt <- hGetLine handle
                  case words nxt of --split what is in message nxt into list of words
                    ["HELO", "BASE_TEST"] -> do
                              sendHandle $ "HELO text\nIP: 0\nPort: " ++ (show portNum) ++ "\nStudentID: 14313812\n"
                              printf "Sending: Helo text\nIP: 0\nPort: portNum\nStudentID: 14313812\n"
                              readNxt
                    ["JOIN_CHATROOM:", roomName] -> do
                              arguments <- getArgs (3) -- get info from join message
                              case map words arguments of -- get details of join
                                    [["CLIENT_IP:",_],["PORT:",_],["CLIENT_NAME:",name]] -> do
                                          printf "joining chatroom\n"
                                          client <- createClient name handle (hash name) -- name may not be unique so use hash for client ID
                                          joinChatRoom client server roomName
                                          runClient server client `finally` (removeClient server client >> return ()) -- run until client removed from chat
                                    _ -> readNxt --if something else then get next message (for case of blank message)
                    ["KILL_SERVICE"] -> do 
                        printf "Killing client\n"
                        hPutStrLn handle "see ya" >> return ()

                    _ -> do
                        putStrLn $ "words are: " ++ show nxt ++ "\n"
                        readNxt

                  where
                        sendHandle = hPutStrLn handle
                        getArgs n = replicateM n (hGetLine handle)
                        

                              
  runClient :: Server -> Client -> IO()
  runClient serv client@Client{..} = do
      printf "running client\n"
      race server recieve -- concurrently do server and recieve
      return ()
      where 
            recieve = forever $ do
                  nxt <- hGetLine clientHandle
                  case words nxt of
                        ["JOIN_CHATROOM:",roomName] -> do
                              printf "running client joining chat\n"
                              restOfMsg <- getArgs (3)
                              sendToRoom restOfMsg roomName
                        ["LEAVE_CHATROOM:",roomID] -> do
                              printf "running client leaving chat\n"
                              restOfMsg <- getArgs (2)
                              sendToRoom restOfMsg roomID
                        ["DISCONNECT:",ip] -> do
                              printf "running client disconnect\n"
                              restOfMsg <- getArgs (2)
                              sendToRoom restOfMsg ip
                        ["CHAT:",roomID]-> do
                              printf "running client chat\n"
                              restOfMsg <- getArgs (3)
                              sendToRoom restOfMsg roomID
                        ["KILL_SERVICE"] -> do
                              printf "running client kill service\n"
                              sendToRoom [killSERV] killSERV
                              return()
                        _ -> do
                              printf "error run client\n"
                              sendErrorToRoom 
                              
                        where 
                              getArgs n = replicateM n $ hGetLine clientHandle
                              sendToRoom msg roomID = atomically $ sendMsg client $ Command (map words msg) roomID
                              sendErrorToRoom  = atomically $ sendMsg client $ Error "Error 1" "No info in message"
  
            server = join $ atomically $ do 
                  msg <- readTChan clientChan
                  return $ do  --- currently a mess, trying to figure out what passes to handleMsg
                        printf "handeling message\n"
                        --putStrLn $ "msg is: "
                        --let arg = hGetLine msg
                        --putStrLn $ show arg ++ "\n"
                        continue <- handleMsg serv client msg
                        when continue $ server


  removeClient :: Server -> Client -> IO() 
  removeClient serv client@Client{..} = do 
      roomsRemove <- atomically $ readTVar serv
      let rooms = Prelude.map (\room -> roomName room)(M.elems roomsRemove)
      mapM_ (\room -> leave room) rooms
      where
            leave room = do 
                  leaveChatroom client serv (hash room) >> putStrLn (clientName ++ " removed from " ++ room)

  newChatroom :: Client -> String -> STM Room
  newChatroom joiningClient@Client{..} room = do
      clientList <- newTVar $ M.insert clientID joiningClient M.empty
      return Room { roomName = room
                  , roomID = hash room
                  , clients = clientList
                  }


  joinChatRoom ::  Client -> Server -> String -> IO()
  joinChatRoom clientJoining@Client{..} serverRooms roomName = atomically $ do
      roomList <- readTVar serverRooms
      case M.lookup (hash roomName) roomList of --check if that chatroom exists
            Nothing -> do
                  room <- newChatroom clientJoining roomName
                  let addRoomList = M.insert (roomID room) room roomList
                  writeTVar serverRooms addRoomList --add new chatroom to list of chatrooms
                  send (roomID room)
            Just a -> do
                  clientList <- readTVar (clients a)
                  let addClientList = M.insert clientID clientJoining clientList
                  writeTVar (clients a) addClientList
                  send (roomID a)
            where
                  send ref = sendMsg clientJoining (Tell $ "JOINED_CHATROOM: "++roomName++"\nSERVER_IP: 0.0.0.0\nPORT: 0\nROOM_REF: " ++ show ref ++"\nJOIN_ID: " ++ show (ref+clientID)++ "\n") --no port as udp


  leaveChatroom :: Client -> Server -> Int -> IO()
  leaveChatroom client@Client{..} server roomID = do
      rooms <- atomically $ readTVar server
      case M.lookup roomID rooms of 
            Nothing -> putStrLn "There is no room with that name" 
            Just a -> do
                  removeClientfrmRoom 
                  sendMsgtoRoom leaveMsg a
                  putStrLn $ clientName ++ " left " ++ (roomName a)
                  where
                        removeClientfrmRoom = atomically $ do 
                              clientList <- readTVar (clients a)
                              let newClientList = M.delete (hash clientName) clientList
                              writeTVar (clients a) newClientList
                              sendMsg client (Tell $ "LEFT_CHATROOM: " ++ (show roomID) ++ "\nJOIN_ID: " ++ (show $ clientID + roomID) ++ "\n")
                        leaveMsg = Broadcast $ "User Left\n" ++ clientName ++ " has left the building\n\n"

  deleteChatroom :: Server -> Int -> IO()
  deleteChatroom serv refID = atomically $ do
      roomsList <- readTVar serv
      case M.lookup refID roomsList of 
            Nothing -> return ()
            Just a -> do
                  let newroomsList = M.delete refID roomsList
                  writeTVar serv newroomsList
