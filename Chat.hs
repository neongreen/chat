module Chat where

import           Control.Concurrent
import           Control.Concurrent.Chan
import           Control.Monad
import           Control.Monad.Fix (fix)
import           Data.IORef
import           Data.List (isPrefixOf)
import qualified Data.Map as M
import           Data.Time.Format
import           Data.Time.LocalTime
import           Network.Socket
import           System.IO
import           System.Locale (defaultTimeLocale)

type Msg = (Int,ZonedTime,String,String)
type Users = IORef (M.Map Int String)

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
    nicksRef <- newIORef M.empty
    mainLoop sock chan 0 nicksRef

mainLoop :: Socket -> Chan Msg -> Int -> Users -> IO ()
mainLoop sock chan id nref = do
    conn <- accept sock
    forkIO (runConn conn chan id nref)
    putStrLn "connection attempt"
    mainLoop sock chan (id+1) nref

runConn :: (Socket, SockAddr) -> Chan Msg -> Int -> Users -> IO ()
runConn (sock, _) chan id nref = do
    let broadcast nick msg = do
          t <- getZonedTime
          let time = formatTime defaultTimeLocale "%T" t
              line = "["++time++"] " ++ nick ++ ": " ++ msg
          writeChan chan (id,t,nick,line)
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
    hPutStr hdl "enter a nickname: "
    nick <- fix $ \loop -> do
        nicks <- readIORef nref
        line <- liftM init (hGetLine hdl)
        let isTaken = line `elem` M.elems nicks
        if isTaken
          then hPutStr hdl "taken! try again: " >> loop
          else modifyIORef' nref (M.insert id line) >> return line
    broadcast nick ("[user " ++ show id ++ " connected]")
    chan' <- dupChan chan
    -- fork off thread for reading from the duplicated channel
    forkIO $ fix $ \loop -> do
        (i,t,n,line) <- readChan chan'
        if i /= id
           then hPutStrLn hdl line
           else return ()
        loop
    -- read lines from socket and echo them back to the user
    fix $ \loop -> do
        line <- liftM init (hGetLine hdl)
        broadcast nick line
        loop
