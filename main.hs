module Main where

import Network.Socket
import System.IO
import Control.Exception
import Control.Concurrent
import Control.Monad (when)
import Control.Monad.Fix (fix)

type Msg = (Int, String)

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




    