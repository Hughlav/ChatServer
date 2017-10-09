module Main where

import Network.Socket
import System.IO
import Control.Concurrent

main :: IO()
main = do
    sock <- socket AF_INET Stream 0 -- create a socket
    setSocketOption sock ReuseAddr 1 -- makes socket reusable
    bind sock (SockAddrInet 4242 iNADDR_ANY) -- listen on port 4242
    listen sock 2 --  max 2 connetions
    mainLoop sock

mainLoop :: Socket -> IO()
mainLoop sock = do 
    conn <- accept sock -- accept a connection 
    forkIO (runConn conn) -- give each connection its own thread
    print "test"
    mainLoop sock -- repeat

runConn :: (Socket, SockAddr) -> IO()
runConn (sock,_) = do
    print "test"
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
    hPutStrLn hdl "Hello!" -- send message on handle (socket)
    
    hClose hdl -- close socket when done with it

-- on part 5: https://wiki.haskell.org/Implement_a_chat_server


    