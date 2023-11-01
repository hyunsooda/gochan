{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Gchan.GchanReceiveOnly
  ( ReceivableGchan (..)
  , ReceivableImpl (..)
  , ReceivableImplT
  , initRG
  ) where

import Data.Kind (Type)
import Gchan.Gchan

class Show a => ReceivableGchan a where
  type ChanTyp a :: Type
  data ReceivableImpl a :: Type

  -- blocking APIs
  (<--) :: ReceivableImpl a -> IO (Either ErrGchan (ChanTyp a))

  -- non-blocking APIs
  chanSize :: ReceivableImpl a -> IO Int

type ReceivableImplT a = ReceivableImpl (ReceiveGchan (Gchan a))

instance Show a => ReceivableGchan (ReceiveGchan (Gchan a)) where
  type ChanTyp (ReceiveGchan (Gchan a)) = a
  data ReceivableImpl (ReceiveGchan (Gchan a)) = ReceivableImplChan (ReceiveGchan (Gchan a))

  (<--) (ReceivableImplChan (ReceiveGchan gchan)) = receiveG gchan

  chanSize (ReceivableImplChan (ReceiveGchan gchan)) = chanSizeG gchan

initRG :: Int -> IO (ReceivableImplT a)
initRG cap = do
  gchan <- initGchan cap
  pure . ReceivableImplChan $ ReceiveGchan gchan
