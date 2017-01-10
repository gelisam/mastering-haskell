{-# LANGUAGE FlexibleContexts, FlexibleInstances, GADTs #-}
module Main where
import Data.MultiSet as MultiSet
import Test.QuickCheck

-- |
-- prop> stubborn ReadI
-- prop> stubborn (WriteI True)
-- prop> stubborn IncrementF
-- prop> stubborn FreezeF
-- prop> stubborn IncrementE
-- prop> stubborn ReadE
stubborn :: ( Arbitrary (SomeR c), Ord (SomeR c), Show (SomeR c)
            , LVish c, Eq a )
         => c a -> Property
stubborn c = property $ \cs (SomeR c')
          -> ( result c cs /= Nothing
            && isAllowed c' cs
            && isAllowed c (insert (SomeR c') cs)
            && isAllowed c' (insert (SomeR c) cs)
             ) `implies` (
               result c (insert (SomeR c') cs)
            == result c cs
             )














implies :: Bool -> Bool -> Bool
implies x y = y || not x


isDisallowed :: LVish c => c a -> MultiSet (SomeR c) -> Bool
isDisallowed c = not . isAllowed c



class LVish c where
  isAllowed :: c r -> MultiSet (SomeR c) -> Bool
  result    :: c r -> MultiSet (SomeR c) -> Maybe r

data SomeR c where
  SomeR :: c r -> SomeR c


data CmdI a r where
  ReadI  :: CmdI a a
  WriteI :: a -> CmdI a ()

data CmdF r where
  IncrementF :: CmdF ()
  FreezeF    :: CmdF Int

data CmdE a where
  IncrementE :: CmdE ()
  ReadE      :: CmdE Int


instance Eq a => Eq (SomeR (CmdI a)) where
  SomeR ReadI      == SomeR ReadI       = True
  SomeR (WriteI x) == SomeR (WriteI x') = x == x'
  _                == _                 = False

instance Eq (SomeR CmdF) where
  SomeR IncrementF == SomeR IncrementF = True
  SomeR FreezeF    == SomeR FreezeF    = True
  _                == _                = False

instance Eq (SomeR CmdE) where
  SomeR IncrementE == SomeR IncrementE = True
  SomeR ReadE      == SomeR ReadE      = True
  _                == _                = False


instance Ord a => Ord (SomeR (CmdI a)) where
  SomeR ReadI      <= SomeR ReadI       = True
  SomeR ReadI      <= SomeR (WriteI _)  = True
  SomeR (WriteI x) <= SomeR (WriteI x') = x <= x'
  _                <= _                 = False

instance Ord (SomeR CmdF) where
  SomeR IncrementF <= SomeR IncrementF = True
  SomeR IncrementF <= SomeR FreezeF    = True
  SomeR FreezeF    <= SomeR FreezeF    = True
  _                <= _                = False

instance Ord (SomeR CmdE) where
  SomeR IncrementE <= SomeR IncrementE = True
  SomeR IncrementE <= SomeR ReadE      = True
  SomeR ReadE      <= SomeR ReadE      = True
  _                <= _                = False


app_prec :: Int
app_prec = 10

instance Show a => Show (SomeR (CmdI a)) where
  showsPrec d (SomeR ReadI)      = showParen (d > app_prec)
                                 $ showString "SomeR ReadI"
  showsPrec d (SomeR (WriteI x)) = showParen (d > app_prec)
                                 $ showString "SomeR (WriteI "
                                 . showsPrec (app_prec+1) x
                                 . showString ")"

instance Show (SomeR CmdF) where
  showsPrec d (SomeR IncrementF) = showParen (d > app_prec)
                                 $ showString "SomeR IncrementF"
  showsPrec d (SomeR FreezeF)    = showParen (d > app_prec)
                                 $ showString "SomeR FreezeF"

instance Show (SomeR CmdE) where
  showsPrec d (SomeR IncrementE) = showParen (d > app_prec)
                                 $ showString "SomeR IncrementE"
  showsPrec d (SomeR ReadE)      = showParen (d > app_prec)
                                 $ showString "SomeR ReadE"


instance Ord a => LVish (CmdI a) where
  isAllowed ReadI      = const True
  isAllowed (WriteI x) = MultiSet.null . MultiSet.filter go
    where go (SomeR (WriteI x')) = x' /= x
          go _                   = False
  result ReadI cs = do
    SomeR (WriteI x):_ <- return
                        $ MultiSet.elems
                        $ MultiSet.deleteAll (SomeR ReadI) cs
    return x
  result (WriteI _) _ = return ()

instance LVish CmdF where
  isAllowed IncrementF = MultiSet.notMember (SomeR FreezeF)
  isAllowed FreezeF    = const True
  result IncrementF = const $ return ()
  result FreezeF    = return . MultiSet.occur (SomeR IncrementF)

instance LVish CmdE where
  isAllowed IncrementE = const True
  isAllowed ReadE      = const True
  result IncrementE = const $ return ()
  result ReadE      = return . MultiSet.occur (SomeR IncrementE)



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

instance Arbitrary a => Arbitrary (SomeR (CmdI a)) where
  arbitrary = oneof [ pure (SomeR ReadI)
                    , SomeR . WriteI <$> arbitrary
                    ]

instance Arbitrary (SomeR CmdF) where
  arbitrary = oneof [ pure (SomeR IncrementF)
                    , pure (SomeR FreezeF)
                    ]

instance Arbitrary (SomeR CmdE) where
  arbitrary = oneof [ pure (SomeR IncrementE)
                    , pure (SomeR ReadE)
                    ]


main :: IO ()
main = return ()
