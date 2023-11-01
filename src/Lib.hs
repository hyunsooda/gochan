
module Lib where



---------------------------------

-- module Lib
--     ( ErrChan (..)
--     , initChan
--     , (-->)
--     , (<--)
--     , range
--     , close
--     , chanSize
--     ) where

-- import Data.Kind (Type)
-- import Control.Concurrent
-- import Control.Monad (when)
-- import Data.IORef
-- import Chan

-- data HChan a = HChan { ch :: Chan a,         -- channel
--                        empty :: MVar Bool,   -- is channel empty
--                        closed :: MVar Bool,  -- is channel closed
--                        capacity :: Int,      -- channel capacity
--                        queue :: IORef [a]    -- channel queue
--                      }

-- data ErrChan
--   = ChanClosed
--   | ChanFull
--   deriving (Show, Eq)

-- instance Show a => Show (HChan a) where
--   show _ = ""

-- instance Show a => Show (Impl (HChan a)) where
--   show _ = ""

-- class Show a => Channable a where
--   data Impl a :: Type
--   type ChanTyp a :: Type

--   -- blocking APIs
--   (-->) :: ChanTyp a -> Impl a -> IO (Either ErrChan ()) -- send
--   (<--) :: Impl a -> IO (Either ErrChan (ChanTyp a))     -- receive
--   range :: Impl a -> (ChanTyp a -> b) -> IO (Either ErrChan [b])

--   -- non-blocking APIs
--   close :: Impl a -> IO ()
--   chanSize :: Impl a -> IO Int

-- instance Show a => Channable (HChan a) where
--   type ChanTyp (HChan a) = a
--   data Impl (HChan a) = ImplChan (HChan a)

--   (-->) :: ChanTyp (HChan a) -> Impl (HChan a) ->  IO (Either ErrChan ())
--   val --> implChan@(ImplChan hchan) = do
--     isClosed <- readMVar (closed hchan)
--     if isClosed then closeErr
--                 else update
--     where
--       closeErr = pure . Left $ ChanClosed
--       fullErr = pure . Left $ ChanFull
--       success = pure (Right ())
--       update = do
--         cs <- chanSize implChan
--         if cs == capacity hchan then fullErr
--         else do
--           isEmpty <- readMVar (empty hchan)
--           if isEmpty then do
--             writeChan (ch hchan) val
--             swapMVar (empty hchan) False >> success
--           else
--             modifyIORef' (queue hchan) (flip (++) [val]) >> success

--   (<--) implChan@(ImplChan hchan) = do
--     isClosed <- readMVar (closed hchan)
--     if isClosed then closeErr
--                 else readCell
--     where
--       closeErr = pure . Left $ ChanClosed
--       readCell = do
--         val <- readChan (ch hchan)
--         cs <- (flip (-) 1) <$> chanSize implChan
--         when (cs == 0) makeChanEmpty
--         when (cs > 0) pushChan
--         pure . Right $ val

--       makeChanEmpty = swapMVar (empty hchan) True >> pure ()
--       pushChan = do
--         q <- readIORef (queue hchan)
--         writeIORef (queue hchan) (drop 1 q)
--         writeChan (ch hchan) (head q)

--   range implChan@(ImplChan hchan) iter = do
--     isClosed <- readMVar (closed hchan)
--     if isClosed then closeErr
--     else do
--       output <- goRange []
--       pure . Right $ output

--     where
--       closeErr = pure . Left $ ChanClosed
--       goRange acc = do
--         r <- (<--) implChan
--         case r of
--           Left _  -> pure acc
--           Right d -> do
--             cs <- chanSize implChan
--             let appended = acc ++ [iter d]
--             if cs == 0 then pure appended
--                        else goRange appended

--   chanSize (ImplChan hchan) = do
--     isEmpty <- readMVar (empty hchan)
--     if isEmpty then pure 0
--     else do
--       q <- readIORef (queue hchan)
--       pure (length q + 1)

--   close (ImplChan hchan) = swapMVar (closed hchan) True >> pure ()

-- initChan :: Int -> IO (Impl (HChan a))
-- initChan cap = do
--   ch <- newChan
--   empty <- newMVar True
--   closed <- newMVar False
--   q <- newIORef []
--   pure . ImplChan $ HChan ch empty closed cap q
