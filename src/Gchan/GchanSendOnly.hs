{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Gchan.GchanSendOnly
  ( SendableGchan (..)
  , SendableImpl (..)
  , SendableImplT
  ) where

import Data.Kind (Type)
import Gchan.Gchan

class Show a => SendableGchan a where
  type ChanTyp a :: Type
  data SendableImpl a :: Type

  -- blocking APIs
  (-->) :: ChanTyp a -> SendableImpl a -> IO (Either ErrGchan ())

  -- non-blocking APIs
  close :: SendableImpl a -> IO ()
  chanSize :: SendableImpl a -> IO Int

type SendableImplT a = SendableImpl (SendGchan (Gchan a))

instance Show a => SendableGchan (SendGchan (Gchan a)) where
  type ChanTyp (SendGchan (Gchan a)) = a
  data SendableImpl (SendGchan (Gchan a)) = SendableImplChan (SendGchan (Gchan a))

  val --> (SendableImplChan (SendGchan gchan)) = val `sendG` gchan

  close (SendableImplChan (SendGchan gchan)) = closeG gchan
  chanSize (SendableImplChan (SendGchan gchan)) = chanSizeG gchan
