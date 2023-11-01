import Test.Tasty
import Test.Tasty.Hspec
import Test.Hspec
import System.IO (hSetBuffering, stdout, BufferMode(..))

import GchanSpec
import TypeWrapSpec
import ConcurrencySpec

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  specs <- concat <$> mapM testSpecs
    [specGchan, specChanTypWrapping, specConcurrencySafe]
  defaultMain (testGroup "All" specs)
  pure ()
