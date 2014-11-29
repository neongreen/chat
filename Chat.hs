module Chat where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Chan
import           Control.Monad
import           Control.Monad.Fix (fix)
import           Data.IORef
import           Data.List (isPrefixOf,isInfixOf)
import           Data.List.Split (chunksOf)
import           Data.List.Utils
import qualified Data.Map as M
import           Data.Maybe (fromJust)
import qualified Data.Text as T
import           Data.Time.Format
import           Data.Time.LocalTime
import           Graphics.Vty
import           Graphics.Vty.Widgets.All
import           Network
import           System.Exit
import           System.IO
import           System.Locale (defaultTimeLocale)

type Msg = (Int,ZonedTime,String,String)

type Users = IORef (M.Map Int String)

-- | Start the server.
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

-- | Connect to the server and run the client.
runClient :: IO ()
runClient = do
  hdl <- runClientConn "localhost"
  inputChan  <- newChan
  outputChan <- newChan
  forkIO $ forever $ do
    line <- hGetLine hdl
    writeChan inputChan line
  forkIO $ forever $ do
    line <- readChan outputChan
    hPutStrLn hdl line
  runClientGUI (inputChan, outputChan)

-- | Just connect to the server.
runClientConn :: HostName -> IO Handle
runClientConn host = do
  hdl <- connectTo host (PortNumber 4242)
  hSetBuffering hdl NoBuffering
  return hdl

-- | Given channels to get messages from and send messages to, start the
-- client GUI.
runClientGUI :: (Chan String, Chan String) -> IO ()
runClientGUI (input, output) = do
  -- Create some widgets.
  messages <- newList 1             -- List of messages.
  newMessage <- editWidget          -- Editbox for user's message.
  box <- vBox messages newMessage   -- Container for messages + editbox.
  ui <- centered box                -- No idea really.
  fg <- newFocusGroup
  addToFocusGroup fg newMessage
  c <- newCollection
  addToCollection c ui fg
  -- Handle Enter in the editbox.
  newMessage `onActivate` \this -> do
    msg <- getEditText this
    -- If the command is “/quit”, well, we quit.
    when (msg == T.pack "/quit") exitSuccess
    writeChan output (T.unpack msg)
    setEditText this (T.pack "")
  -- Read server messages as they come and add them to the list.
  forkIO . forever $ readChan input >>= \m -> do
    let addMessage line list = do
          text <- textWidget wrap line
          addToList list line text
          scrollDown list
    schedule $ do
      width <- regionWidth <$> getCurrentSize messages
      -- Break the gotten message in chunks and output them one by one.
      forM_ (chunksOf width m) $ \line ->
        addMessage (T.pack line) messages
      -- Wait a bit... for reasons unknown.
      threadDelay 10000
  runUi c defaultContext
