module GchanSpec where

import Test.Tasty
import Test.Tasty.Hspec
import Test.Hspec

import Control.Monad (forM_, forM)
import Data.List (intercalate)
import Data.Either (fromRight)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async

import Gchan.Gchan

specGchan :: Spec
specGchan =
  describe "gochan test" $
    gchanSpecTest

gchanSpecTest :: Spec
gchanSpecTest =
  describe "channel functionality" $ do
    it "basic behavior test" $ do
      let cap = 5
      cap >= length dummyInputs `shouldBe` True
      got <- sendReceiveTest dummyInputs cap
      got `shouldBe` (mconcat dummyInputs)

    it "expected blocking test" $ do
      let timer = threadDelay oneSec
          oneSec = 1000000
      timerFinished <- race blockingTest timer
      timerFinished `shouldBe` Right ()

    it "channel range test" $ do
      let dummyStr = "dummy"
      output <- rangeTest dummyStr
      output `shouldBe` (intercalate dummyStr dummyInputs)

    it "channel full test" $ do
      chanFullTest

    it "channel close test" $ do
      chanCloseTest

    it "channel size test" $ do
      chanSizeTest

dummyInputs :: [String]
dummyInputs = ["data1", "data2", "data3"]

sendReceiveTest :: [String] -> Int -> IO String
sendReceiveTest inputs cap = do
  hchan <- initGchan cap
  forM_ inputs (flip sendG hchan)
  received <- forM inputs $ \_ -> do
    x <- receiveG hchan
    pure $ fromRight "" x
  pure (mconcat received)

blockingTest :: IO ()
blockingTest = do
  hchan <- initGchan 2
  _ <- "data1" `sendG` hchan
  _ <- receiveG hchan
  _ <- receiveG hchan
  pure ()

chanFullTest :: IO ()
chanFullTest = do
  hchan <- initGchan 2
  sent1 <- "data1" `sendG` hchan
  sent1 `shouldBe` (Right ())
  sent2 <- "data2" `sendG` hchan
  sent2 `shouldBe` (Right ())
  sent3 <- "data3" `sendG` hchan
  sent3 `shouldBe` (Left ChanFull)
  pure ()

chanCloseTest :: IO ()
chanCloseTest = do
  hchan <- initGchan 2
  closeG hchan
  sent <- "data1" `sendG` hchan
  sent `shouldBe` (Left ChanClosed)
  pure ()

chanSizeTest :: IO ()
chanSizeTest = do
  hchan <- initGchan 5
  _ <- "dummy" `sendG` hchan
  cs <- chanSizeG hchan
  cs `shouldBe` 1

  _ <- "dummy" `sendG` hchan
  cs <- chanSizeG hchan
  cs `shouldBe` 2

  _ <- "dummy" `sendG` hchan
  cs <- chanSizeG hchan
  cs `shouldBe` 3

  receiveG hchan
  cs <- chanSizeG hchan
  cs `shouldBe` 2
  pure ()

rangeTest :: String -> IO String
rangeTest dummyStr = do
  hchan <- initGchan 5
  "data1" `sendG` hchan
  "data2" `sendG` hchan
  "data3" `sendG` hchan
  output <- rangeG hchan (\x -> x)
  pure $ intercalate dummyStr (fromRight [] output)
