module Main where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Chan
import           Control.Monad
import           Data.List.Split (chunksOf)
import qualified Data.Text as T
import           Graphics.Vty
import           Graphics.Vty.Widgets.All
import           Network
import           System.Exit
import           System.IO

main :: IO ()
main = do
  hdl <- connect "localhost"
  inputChan  <- newChan
  outputChan <- newChan
  forkIO $ forever $ do
    line <- hGetLine hdl
    writeChan inputChan line
  forkIO $ forever $ do
    line <- readChan outputChan
    hPutStrLn hdl line
  runUI (inputChan, outputChan)

-- | Connect to the server.
connect :: HostName -> IO Handle
connect host = do
  hdl <- connectTo host (PortNumber 4242)
  hSetBuffering hdl NoBuffering
  return hdl

-- | Given channels to get messages from and send messages to, start the
-- client GUI.
runUI :: (Chan String, Chan String) -> IO ()
runUI (input, output) = do
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
