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

data UI = UI
  { addMessage :: String -> IO ()
  }

main :: IO ()
main = do
  hdl <- connect "localhost"
  inputChan <- newChan
  ui <- runUI inputChan
  -- Handle incoming messages.
  forkIO $ forever $ do
    line <- hGetLine hdl
    addMessage ui line
  -- Handle user input.
  forever $ do
    line <- readChan inputChan
    -- If the command is “/quit”, well, we quit.
    when (line == "/quit") exitSuccess
    -- Otherwise we send the message to the server.
    hPutStrLn hdl line
  return ()

-- | Connect to the server.
connect :: HostName -> IO Handle
connect host = do
  hdl <- connectTo host (PortNumber 4242)
  hSetBuffering hdl NoBuffering
  return hdl

-- | Create the client UI.
runUI :: Chan String  -- ^ A channel to send lines of input to.
      -> IO UI        -- ^ The resulting structure contains various functions
                      --   to operate on the created UI.
runUI output = do
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
    line <- getEditText this
    writeChan output (T.unpack line)
    setEditText this (T.pack "")
  let addLine line list = do
        text <- textWidget wrap line
        addToList list line text
        scrollDown list
  let addMessage msg = schedule $ do
        width <- regionWidth <$> getCurrentSize messages
        -- Break the gotten message in chunks and output them one by one.
        forM_ (chunksOf width msg) $ \line ->
          addLine (T.pack line) messages
        -- Wait a bit... for reasons unknown.
        threadDelay 10000
  forkIO $ runUi c defaultContext
  return $ UI { addMessage = addMessage }
