module Lib
     where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Text.ABNF.ABNF.Types as A
import Data.Text (Text, singleton, unpack)
import Data.Maybe

abnfGen :: Rule -> Gen Text
abnfGen (Rule _ Equals alts) = sumGen alts
abnfGen (Rule _ Adds _) = error "canonicalize the rule first!"

sumGen :: SumSpec -> Gen Text
sumGen (SumSpec prods) = Gen.choice (map (Gen.small . prodGen) prods)

prodGen :: ProductSpec -> Gen Text
prodGen (ProductSpec reps) = do
  reps' <- mapM repGen reps

  return (mconcat reps')

repGen :: Repetition -> Gen Text
repGen (Repetition reps element) = do
  let range = repeatRange reps

  mconcat <$> Gen.list range (elemGen element)

  where repeatRange (Repeat lower upper) = Range.linear lower (fromMaybe 100 upper)

elemGen :: Element -> Gen Text
elemGen (LiteralElement lit) = litGen lit
elemGen (OptionElement (A.Group alts)) = fromMaybe mempty <$> Gen.maybe (sumGen alts)
elemGen (GroupElement (A.Group alts)) = sumGen alts
elemGen (RuleElement' ident) = error $ "Found non-canonical rule element: " ++ unpack ident
elemGen (RuleElement r) = abnfGen r

litGen :: Literal -> Gen Text
litGen (CharLit text) = pure text
litGen (NumLit num) = numLitGen num

numLitGen :: NumLit -> Gen Text
numLitGen (IntLit digs) = singleton . toEnum <$> Gen.element digs
numLitGen (RangeLit lower upper) = singleton . toEnum <$> Gen.integral (Range.linear lower upper)
