module Main where

import Amazonka.S3 qualified as S3
import AppServer (toS3Key)
import Data.Time (UTCTime (UTCTime), fromGregorian)
import Test.Hspec (describe, hspec, it, shouldBe)

main :: IO ()
main = hspec $ do
  describe "toS3Key" $ do
    it "formats single-digit month/day with zero-padding" $ do
      let timestamp = UTCTime (fromGregorian 2025 1 7) (10 * 3600)
          freePlayer = "aragorn"
          shadowPlayer = "arwen"
          expected = S3.ObjectKey "2025/01/07/2025-01-07_100000_FP_aragorn_SP_arwen.log"
      toS3Key timestamp freePlayer shadowPlayer `shouldBe` expected
