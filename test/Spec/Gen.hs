module Spec.Gen where
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- generators to create Rules

import Text.ABNF.ABNF.Types as A

genLit :: Gen Literal
genLit = Gen.choice
  [ CharLit <$> Gen.text (Range.linear 5 10) Gen.alphaNum
  , NumLit <$> genNumLit
  ]

genNumLit :: Gen NumLit
genNumLit = Gen.choice
  [ IntLit <$> Gen.list (Range.linear 1 10) (Gen.int $ Range.linear 0 maxBound)
  , RangeLit <$> (Gen.int $ Range.linear 0 maxBound) <*> (Gen.int $ Range.linear 0 maxBound)
  ]

genElem :: Gen Element
genElem = Gen.recursive Gen.choice
  [ LiteralElement <$> genLit
  ]
  [ OptionElement <$> genGroup
  , GroupElement <$> genGroup
  ]

genGroup :: Gen A.Group
genGroup = A.Group <$> genSum

genSum :: Gen SumSpec
genSum = SumSpec <$> Gen.list (Range.linear 1 10) genProd

genProd :: Gen ProductSpec
genProd = ProductSpec <$> Gen.list (Range.linear 1 10) genReps

genReps :: Gen Repetition
genReps = Repetition <$> genRepeats <*> genElem
  where genRepeats = Repeat <$> (Gen.int $ Range.linear 0 10) <*> Gen.maybe (Gen.int (Range.linear 11 20))
