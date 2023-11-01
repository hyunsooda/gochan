module TypeWrapSpec where

import Test.Tasty
import Test.Tasty.Hspec
import Test.Hspec

import Gchan.Gchan
import qualified Gchan.GchanBidirectional as BG
import qualified Gchan.GchanSendOnly as SG
import qualified Gchan.GchanReceiveOnly as RG

specChanTypWrapping :: Spec
specChanTypWrapping =
  describe "Type wrapping test" $ do
    it "Convert bidirectional to sendonly" $ do
      wrapSendOnlyTest
    
    it "Convert bidirectional to receiveonly" $ do
      wrapReceiveOnlyTest

wrapSendOnlyTest :: IO ()
wrapSendOnlyTest = do
  bg <- BG.initBG 5
  "data1" BG.--> bg
  cs <- BG.chanSize bg
  cs `shouldBe` 1

  sg <- BG.wrap2SendOnly bg
  "data2" SG.--> sg
  cs <- BG.chanSize bg
  cs `shouldBe` 2
  cs <- SG.chanSize sg -- Wrapped struct have the same reference
  cs `shouldBe` 2
  pure ()

wrapReceiveOnlyTest :: IO ()
wrapReceiveOnlyTest = do
  bg <- BG.initBG 5
  "data1" BG.--> bg
  cs <- BG.chanSize bg
  cs `shouldBe` 1

  rg <- BG.wrap2ReceiveOnly bg
  _ <- (RG.<--) rg 
  cs <- RG.chanSize rg
  cs `shouldBe` 0
  pure ()
