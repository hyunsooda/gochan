import Test.Tasty
import Test.Tasty.Hspec
import Test.Hspec

import Control.Monad (forM_, forM)
import Data.Either (fromRight)
import System.IO (hSetBuffering, stdout, BufferMode(..))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async

import Lib

specChan :: Spec
specChan =
  describe "gochan test" $
    spec_test

spec_test :: Spec
spec_test =
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

    it "channel full test" $ do
      chanFullTest

    it "channel close test" $ do
      chanCloseTest

    it "channel size test" $ do
      chanSizeTest

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  tree <- testSpec "gochan test" specChan
  defaultMain tree

dummyInputs :: [String]
dummyInputs = ["data1", "data2", "data3"]

sendReceiveTest :: [String] -> Int -> IO String
sendReceiveTest inputs cap = do
  hchan <- initChan cap
  forM_ inputs (flip (-->) hchan)
  received <- forM inputs $ \_ -> do
    x <- (<--) hchan
    pure $ fromRight "" x
  pure (mconcat received)

blockingTest :: IO ()
blockingTest = do
  hchan <- initChan 2
  _ <- "data1" --> hchan
  _ <- (<--) hchan
  _ <- (<--) hchan
  pure ()

chanFullTest :: IO ()
chanFullTest = do
  hchan <- initChan 2
  sent1 <- "data1" --> hchan
  sent1 `shouldBe` (Right ())
  sent2 <- "data2" --> hchan
  sent2 `shouldBe` (Right ())
  sent3 <- "data3" --> hchan
  sent3 `shouldBe` (Left ChanFull)
  pure ()

chanCloseTest :: IO ()
chanCloseTest = do
  hchan <- initChan 2
  close hchan
  sent <- "data1" --> hchan
  sent `shouldBe` (Left ChanClosed)
  pure ()

chanSizeTest :: IO ()
chanSizeTest = do
  hchan <- initChan 5
  _ <- "dummy" --> hchan
  cs <- chanSize hchan
  cs `shouldBe` 1

  _ <- "dummy" --> hchan
  cs <- chanSize hchan
  cs `shouldBe` 2

  _ <- "dummy" --> hchan
  cs <- chanSize hchan
  cs `shouldBe` 3

  (<--) hchan
  cs <- chanSize hchan
  cs `shouldBe` 2
  pure ()
