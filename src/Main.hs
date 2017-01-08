module Main where
import Data.MultiSet as MultiSet
import Test.QuickCheck

isDisallowed :: LVish c => c -> MultiSet c -> Bool
isDisallowed c = not . isAllowed c

implies :: Bool -> Bool -> Bool
implies x y = y || not x

-- |
-- prop> stubborn ReadI
-- prop> stubborn (WriteI True)
-- prop> stubborn IncrementF
-- prop> stubborn FreezeF
-- ----- stubborn IncrementE
-- ----- stubborn ReadEvenE
stubborn :: (Arbitrary c, Ord c, Show c, LVish c)
         => c -> Property
stubborn c = property $ \cs cs' -> isDisallowed c cs
                         `implies` isDisallowed c (cs `union` cs')




















data CmdI a = ReadI      | WriteI a  deriving (Show, Eq, Ord)
data CmdF   = IncrementF | FreezeF   deriving (Show, Eq, Ord)
data CmdE   = IncrementE | ReadEvenE deriving (Show, Eq, Ord)

class LVish c where
  isAllowed :: c -> MultiSet c -> Bool

instance Ord a => LVish (CmdI a) where
  isAllowed ReadI      = const True
  isAllowed (WriteI x) = MultiSet.null . MultiSet.filter go
    where go (WriteI x') = x' /= x
          go _           = False

instance LVish CmdF where
  isAllowed FreezeF    = const True
  isAllowed IncrementF = MultiSet.notMember FreezeF

instance LVish CmdE where
  isAllowed IncrementE = const True
  isAllowed ReadEvenE  = even . MultiSet.occur IncrementE



instance (Arbitrary a, Ord a) => Arbitrary (MultiSet a) where
  arbitrary = fromList <$> arbitrary
  shrink xs
   -- remove all duplicates
    = if size xs > distinctSize xs then [fromSet $ toSet xs]
                                   else []
   -- remove all copies of one element
   ++ [deleteAll x xs | x <- distinctElems xs]
   -- reduce the number of copies of one element
   ++ [delete x xs | x <- distinctElems xs, occur x xs > 1]

instance Arbitrary a => Arbitrary (CmdI a) where
  arbitrary = oneof [ pure ReadI
                    , WriteI <$> arbitrary
                    ]

instance Arbitrary CmdF where
  arbitrary = oneof [ pure IncrementF
                    , pure FreezeF
                    ]

instance Arbitrary CmdE where
  arbitrary = oneof [ pure IncrementE
                    , pure ReadEvenE
                    ]


main :: IO ()
main = return ()
