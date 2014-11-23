module Chat where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Fix (fix)
import Network.Socket
import System.IO

type Msg = String
 
main :: IO ()
main = do
    -- create socket
    sock <- socket AF_INET Stream 0
    -- make socket immediately reusable - eases debugging.
    setSocketOption sock ReuseAddr 1
    -- listen on TCP port 4242
    bindSocket sock (SockAddrInet 4242 iNADDR_ANY)
    -- allow a maximum of 2 outstanding connections
    listen sock 2
    chan <- newChan
    mainLoop sock chan
 
mainLoop :: Socket -> Chan Msg -> IO ()
mainLoop sock chan = do
    conn <- accept sock
    forkIO (runConn conn chan)
    putStrLn "connection attempt"
    mainLoop sock chan
             
runConn :: (Socket, SockAddr) -> Chan Msg -> IO ()
runConn (sock, _) chan = do
    let broadcast msg = writeChan chan msg
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
    chan' <- dupChan chan
    -- fork off thread for reading from the duplicated channel
    forkIO $ fix $ \loop -> do
        line <- readChan chan'
        hPutStrLn hdl line
        loop
    -- read lines from socket and echo them back to the user
    fix $ \loop -> do
        line <- liftM init (hGetLine hdl) 
        broadcast line
        loop