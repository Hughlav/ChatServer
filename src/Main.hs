{-# LANGUAGE RecordWildCards #-} 
module Main where
  
  import Network.Socket
  import System.IO
  import Control.Exception
  import Control.Concurrent
  import Control.Monad (when)
  import Control.Monad.Fix (fix)
  import Control.Applicative
  import Control.Concurrent.STM
  import Data.Map (Map)
  import qualified Data.Map as M
  import Data.Set (Set)
  import qualified Data.Set as S

  
  {-
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
  
  type ClientName = String
  type RoomName = String


--check
  type ErrorHeading = String
  type ErrorBody = String
  type CmdArgs = [[String]]

  data Message = Notice String
      | Tell ClientName String
      | Broadcast ClientName String
      | Command CmdArgs String
      | Error ErrorHeading ErrorBody



  data Client = Client
      { clientName :: ClientName
      , clientChan :: TChan Message
      , clientHandle :: Handle 
  }
  
  data Room = Room
      { roomName :: RoomName
      , roomID :: Int
      , clients :: TVar (Map Int Client) -- Store list of clients in room
  }
  
  type Msg = (Int, String)
  type RoomList = TVar (Map Int Room) -- Store list of rooms
  
  main :: IO()
  main = do
      print "tst"
      sock <- socket AF_INET Stream 0 -- create a socket
      setSocketOption sock ReuseAddr 1 -- makes socket reusable
      bind sock (SockAddrInet 4242 iNADDR_ANY) -- listen on port 4242
      listen sock 2 --  max 2 connetions
      chan <- newChan
      _ <- forkIO $ fix $ \loop -> do
          (_, _) <- readChan chan
          loop
      mainLoop sock chan 0
      
  
  mainLoop :: Socket -> Chan Msg -> Int -> IO ()
  mainLoop sock chan msgNum = do 
      conn <- accept sock -- accept a connection 
      forkIO (runConn conn chan msgNum) -- give each connection its own thread
      print "test main loop"
      mainLoop sock chan $! msgNum + 1 -- repeat
  
  runConn :: (Socket, SockAddr) -> Chan Msg -> Int -> IO()
  runConn (sock,_) chan msgNum = do
      print "test run conn"
      let broadcast msg = writeChan chan (msgNum, msg)
      hdl <- socketToHandle sock ReadWriteMode
      hSetBuffering hdl NoBuffering
  
      hPutStrLn hdl "What is your name?"
      name <- fmap init (hGetLine hdl)
      broadcast ("-->" ++ name ++ "entered chat.")
      hPutStrLn hdl ("Welcome" ++ name ++ "!")
  
      commLine <- dupChan chan
  
      --fork thread for reading from duplicate channel
      reader <- forkIO $ fix $ \loop -> do
          (nextNum,line) <- readChan commLine
          when (msgNum /= nextNum ) $ hPutStrLn hdl line -- send message on handle (socket)
          loop
      
      -- read lines from the socket and echo to user
      handle (\(SomeException _) -> return()) $ fix $ \loop -> do
          line <- fmap init (hGetLine hdl)
          case line of
              -- If an exception is caught, send a message and break the loop
              "quit" -> hPutStrLn hdl "Bye!"
              -- else, continue looping.
              _      -> broadcast (name ++ ": " ++ line) >> loop
  
      killThread reader                      -- kill after the loop ends
      broadcast ("<-- " ++ name ++ " left.") -- make a final broadcast
      hClose hdl                             -- close the handle
  

--do i need ID
  createClient :: ClientName -> Handle -> IO Client
  createClient name handle = do
      chan <- newTChanIO
      return Client { clientName = name
                    , clientChan = chan
                    , clientHandle = handle
                    }

  sendMsg :: Client -> Message -> STM ()
  sendMsg Client{..} msg = writeTChan clientChan msg -- {..} pattern matching so clientChan is easily accesible 
      
  sendMsgtoRoom :: Message -> Room -> IO ()
  sendMsgtoRoom msg room@Room{..} = atomically $ do 
      clientList <- readTVar clients --return a list of clients in a room from clients TVar
      let roomClients = M.elems clientList --access properties of clients
      mapM_ (\a -> sendMsg a msg) roomClients --send the message to each member in the room
  
  
  
  
  --isInfixOf "Join" msgfromclient --returns true if join is in msg. use this to handle messages. where though?