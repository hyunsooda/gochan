module Gchan.Gchan
    ( Gchan
    , ErrGchan (..)
    , SendReceiveGchan (..)
    , SendGchan (..)
    , ReceiveGchan (..)
    , initGchan
    , sendG
    , receiveG
    , rangeG
    , closeG
    , chanSizeG
    ) where

import Data.Kind (Type)
import Control.Concurrent
import Control.Monad (when)
import Data.IORef

data ErrGchan
  = ChanClosed
  | ChanFull
  deriving (Show, Eq)

data Gchan a = Gchan { ch :: Chan a,         -- channel
                       empty :: MVar Bool,   -- is channel empty
                       closed :: MVar Bool,  -- is channel closed
                       capacity :: Int,      -- channel capacity
                       queue :: IORef [a],   -- channel queue
                       opMutex :: MVar ()    -- operation (read, write) mutex
                     }

instance Show a => Show (Gchan a) where
  show _ = ""

data SendReceiveGchan a = SendReceiveGchan a
  deriving Show
data SendGchan a = SendGchan a
  deriving Show
data ReceiveGchan a = ReceiveGchan a
  deriving Show

-- chanSizeG is not thread-safe
chanSizeG :: Gchan a -> IO Int
chanSizeG gchan = do
  isEmpty <- readMVar (empty gchan)
  if isEmpty then pure 0
  else do
    q <- readIORef (queue gchan)
    pure (length q + 1)

sendG :: a -> Gchan a -> IO (Either ErrGchan ())
val `sendG` gchan = do
  _ <- takeMVar (opMutex gchan)
  isClosed <- readMVar (closed gchan)
  if isClosed then do
    fail <- closeErr
    done >> pure fail
  else do
    successOrFail <- update
    done >> pure successOrFail

  where
    closeErr = pure . Left $ ChanClosed
    fullErr = pure . Left $ ChanFull
    success = pure (Right ())
    done = putMVar (opMutex gchan) ()
    update = do
      cs <- chanSizeG gchan
      if cs == capacity gchan then fullErr
      else do
        isEmpty <- readMVar (empty gchan)
        if isEmpty then do
          writeChan (ch gchan) val
          swapMVar (empty gchan) False >> success
        else do
          modifyIORef' (queue gchan) (flip (++) [val]) >> success

receiveG :: Gchan a -> IO (Either ErrGchan a)
receiveG gchan = do
  _ <- takeMVar (opMutex gchan)
  isClosed <- readMVar (closed gchan)
  if isClosed then do
    fail <- closeErr
    done >> pure fail
  else do
    val <- readCell
    done >> pure val

  where
    closeErr = pure . Left $ ChanClosed
    done = putMVar (opMutex gchan) ()
    readCell = do
      val <- readChan (ch gchan)
      cs <- (flip (-) 1) <$> chanSizeG gchan
      when (cs == 0) makeChanEmpty
      when (cs > 0) pushChan
      pure . Right $ val

    makeChanEmpty = swapMVar (empty gchan) True >> pure ()
    pushChan = do
      q <- readIORef (queue gchan)
      writeIORef (queue gchan) (drop 1 q)
      writeChan (ch gchan) (head q)

rangeG :: Gchan a -> (a -> b) -> IO (Either ErrGchan [b])
rangeG gchan iter = do
  isClosed <- readMVar (closed gchan)
  if isClosed then closeErr
  else do
    output <- goRange []
    pure . Right $ output

  where
    closeErr = pure . Left $ ChanClosed
    goRange acc = do
      r <- receiveG gchan
      case r of
        Left _  -> pure acc
        Right d -> do
          cs <- chanSizeG gchan
          let appended = acc ++ [iter d]
          if cs == 0 then pure appended
                     else goRange appended

closeG :: Gchan a -> IO ()
closeG gchan = swapMVar (closed gchan) True >> pure ()

initGchan :: Int -> IO (Gchan a)
initGchan cap = do
  ch <- newChan
  empty <- newMVar True
  closed <- newMVar False
  opMutex <- newMVar ()
  q <- newIORef []
  pure $ Gchan ch empty closed cap q opMutex
