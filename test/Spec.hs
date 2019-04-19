import Test.Hspec

import qualified Data.Text.IO as T
import qualified Data.Text as T
import Text.ABNF

import Text.Megaparsec

import Hedgehog
import qualified Hedgehog.Gen as Gen

import Lib

import Text.ABNF.ABNF.Types

import Spec.Gen

import HaskellWorks.Hspec.Hedgehog

import System.Timeout
import Control.Monad.IO.Class

main :: IO ()
main = hspec $ do
  describe "abnfGen" $ do
    it "generates valid examples from a grammar" $ do
      grammar <- T.readFile "test/grammars/postal.abnf"

      case (parseABNF "" grammar) of
        Left err -> expectationFailure $ errorBundlePretty err
        Right gram -> do
          let Just gram' = canonicalizeRules (T.pack "postal-address") gram

          putStrLn "omg"
          -- Gen.print (abnfGen gram')
          putStrLn "omg"

          pending

  describe "litGen" $ do
    it "generates literals" $ do
      require $ property $ do
        lit <- forAll $ genLit
        gened <- liftIO $ timeout 100000 (Gen.sample (litGen lit))
        gened /== Nothing

  describe "elemGen" $ do
    it "generates elements" $ do
      require $ property $ do
        elem <- forAll $ genElem
        gened <- liftIO $ timeout 100000 (Gen.sample (elemGen elem))
        gened /== Nothing

  describe "prodGen" $ do
    it "generates products" $ do
      require $ property $ do
        elem <- forAll $ genProd
        gened <- liftIO $ timeout 1000 (Gen.sample (prodGen elem))
        gened /== Nothing

  describe "sumGen" $ do
    it "generates sumucts" $ do
      require $ property $ do
        elem <- forAll $ genSum
        gened <- liftIO $ timeout 1000 (Gen.sample (sumGen elem))
        gened /== Nothing

