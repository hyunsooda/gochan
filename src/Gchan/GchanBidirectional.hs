{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Gchan.GchanBidirectional
  ( BidirectionalGchan (..)
  , BidirectImpl (..)
  , BidirectImplT
  , initBG
  ) where

import Data.Kind (Type)
import Gchan.Gchan
import qualified Gchan.GchanSendOnly as SG
import qualified Gchan.GchanReceiveOnly as RG

class Show a => BidirectionalGchan a where
  type ChanTyp a :: Type
  data BidirectImpl a :: Type

  -- blocking APIs
  (-->) :: ChanTyp a -> BidirectImpl a -> IO (Either ErrGchan ())
  (<--) :: BidirectImpl a -> IO (Either ErrGchan (ChanTyp a))

  range :: BidirectImpl a -> (ChanTyp a -> b) -> IO (Either ErrGchan [b])

  -- non-blocking APIs
  close :: BidirectImpl a -> IO ()
  chanSize :: BidirectImpl a -> IO Int

  wrap2SendOnly :: BidirectImpl a -> IO (SG.SendableImplT (ChanTyp a))
  wrap2ReceiveOnly :: BidirectImpl a -> IO (RG.ReceivableImplT (ChanTyp a))

type BidirectImplT a = BidirectImpl (SendReceiveGchan (Gchan a))

instance Show a => BidirectionalGchan (SendReceiveGchan (Gchan a)) where
  type ChanTyp (SendReceiveGchan (Gchan a)) = a
  data BidirectImpl (SendReceiveGchan (Gchan a)) = BidirectImplChan (SendReceiveGchan (Gchan a))

  val --> (BidirectImplChan (SendReceiveGchan gchan)) = val `sendG` gchan
  (<--) (BidirectImplChan (SendReceiveGchan gchan)) = receiveG gchan

  range (BidirectImplChan (SendReceiveGchan gchan)) iter = rangeG gchan iter

  close (BidirectImplChan (SendReceiveGchan gchan)) = closeG gchan
  chanSize (BidirectImplChan (SendReceiveGchan gchan)) = chanSizeG gchan

  wrap2SendOnly (BidirectImplChan (SendReceiveGchan gchan)) =
    pure . SG.SendableImplChan $ SendGchan gchan

  wrap2ReceiveOnly (BidirectImplChan (SendReceiveGchan gchan)) =
    pure . RG.ReceivableImplChan $ ReceiveGchan gchan

initBG :: Int -> IO (BidirectImplT a)
initBG cap = do
  gchan <- initGchan cap
  pure . BidirectImplChan $ SendReceiveGchan gchan
