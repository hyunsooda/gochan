module ConcurrencySpec where

import Test.Tasty
import Test.Tasty.Hspec
import Test.Hspec

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Monad (forM, forM_)
import Data.Either (fromRight)

import qualified Gchan.GchanBidirectional as BG
import qualified Gchan.GchanSendOnly as SG
import qualified Gchan.GchanReceiveOnly as RG

specConcurrencySafe :: Spec
specConcurrencySafe =
  describe "Concurrecny usage test" $ do
    it "Concurrency write test" $ do
      concurrencyWriteTest 10000 >> pure ()

    it "Concurrency read test" $ do
      concurrencyReadTest 10000

    it "Range test" $ do
      concurrencyRangeTest 10000


concurrencyWriteTest :: Int -> IO (BG.BidirectImplT Int)
concurrencyWriteTest len = do
  bg <- BG.initBG (len * 2)
  concurrently_ (trySend bg) (trySend bg)
  cs <- BG.chanSize bg
  cs `shouldBe` (len * 2)
  pure bg
    where
      trySend bg = forM_ [1..len] (flip (BG.-->) bg)

concurrencyReadTest :: Int -> IO ()
concurrencyReadTest len = do
  bg <- concurrencyWriteTest len
  (readOutput1, readOutput2) <- concurrently (tryRead bg) (tryRead bg)
  length readOutput1 `shouldBe` len
  length readOutput2 `shouldBe` len
    where
      tryRead bg = forM [1..len] (const ((BG.<--) bg))

concurrencyRangeTest :: Int -> IO ()
concurrencyRangeTest len = do
  bg <- BG.initBG len
  ret <- async (trySend bg)
  -- This way is necessay to make sure that write operation is done at lest one
  threadDelay 100
  cs <- BG.chanSize bg
  vals <- BG.range bg (\x -> x)
  length (fromRight [] vals) `shouldBe` len
    where
      trySend bg = forM_ [1..len] (flip (BG.-->) bg)
