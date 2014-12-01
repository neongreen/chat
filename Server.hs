module Main where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Chan
import           Control.Monad.Fix (fix)
import           Data.IORef
import           Data.List (isPrefixOf, isInfixOf)
import           Data.List.Utils (replace)
import           Data.Time.LocalTime
import qualified Data.Map as M
import           Data.Maybe (fromJust)
import           Data.Time.Format
import           Network
import           System.IO
import           System.Locale (defaultTimeLocale)

import           Chat

type Users = IORef (M.Map Int String)

main :: IO ()
main = do
  sock <- listenOn (PortNumber 4242)
  chan <- newChan
  nicksRef <- newIORef M.empty
  mainLoop sock chan 0 nicksRef

mainLoop :: Socket -> Chan Msg -> Int -> Users -> IO ()
mainLoop sock chan id nref = do
  (hdl, _, _) <- accept sock
  hSetBuffering hdl NoBuffering
  forkIO (runConn hdl chan id nref)
  putStrLn "connection attempt"
  mainLoop sock chan (id+1) nref

runConn :: Handle -> Chan Msg -> Int -> Users -> IO ()
runConn hdl chan id nref = do
    let broadcast msg = do
          t <- getZonedTime
          nick <- fromJust . M.lookup id <$> readIORef nref
          let time = formatTime defaultTimeLocale "%T" t
              line = if "/me" `isInfixOf` msg
                        then "["++time++"] " ++ replace "/me" nick msg
                        else "["++time++"] " ++ nick ++ ": " ++ msg
          writeChan chan (id,t,nick,line)
    hPutStrLn hdl "enter a nickname"
    fix $ \loop -> do
        nicks <- readIORef nref
        line <- hGetLine hdl
        let isTaken = line `elem` M.elems nicks
        if isTaken
          then hPutStrLn hdl "taken! try again" >> loop
          else modifyIORef' nref (M.insert id line)
    broadcast ("[user " ++ show id ++ " connected]")
    chan' <- dupChan chan
    -- fork off thread for reading from the duplicated channel
    forkIO $ fix $ \loop -> do
        (i,t,n,line) <- readChan chan'
        hPutStrLn hdl line
        loop
    -- read lines from socket and echo them back to the user
    fix $ \loop -> do
        line <- hGetLine hdl
        if "/nick " `isPrefixOf` line
           then do
             nicks <- readIORef nref
             let new = drop 6 line
                 isTaken = new `elem` M.elems nicks
             if isTaken
                then hPutStrLn hdl "already taken!"
                else modifyIORef' nref (M.insert id new)
           else broadcast line
        loop
