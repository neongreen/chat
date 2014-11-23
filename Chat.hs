module Chat where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Fix (fix)
import Data.Time.Format
import Data.Time.LocalTime
import Network.Socket
import System.IO
import System.Locale (defaultTimeLocale)

type Msg = (Int,ZonedTime,String)

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
    mainLoop sock chan 0

mainLoop :: Socket -> Chan Msg -> Int -> IO ()
mainLoop sock chan id = do
    conn <- accept sock
    forkIO (runConn conn chan id)
    putStrLn "connection attempt"
    mainLoop sock chan (id+1)

runConn :: (Socket, SockAddr) -> Chan Msg -> Int -> IO ()
runConn (sock, _) chan id = do
    let broadcast msg = do
          t <- getZonedTime
          let time = formatTime defaultTimeLocale "%T" t
              line = "["++time++"] " ++ "["++show id++"] " ++ msg
          writeChan chan (id,t,line)
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
    broadcast ("[user " ++ show id ++ " connected]\n")
    chan' <- dupChan chan
    -- fork off thread for reading from the duplicated channel
    forkIO $ fix $ \loop -> do
        (i,t,line) <- readChan chan'
        if i /= id
           then hPutStrLn hdl line
           else return ()
        loop
    -- read lines from socket and echo them back to the user
    fix $ \loop -> do
        line <- liftM init (hGetLine hdl)
        broadcast line
        loop
