{-# Language TypeApplications, DataKinds, TypeInType, KindSignatures #-}

import Data.HeterogeneousListLiterals
import GHC.TypeLits
import Data.Kind
import Data.Dynamic

data HList (a :: [Type]) = HList [Dynamic]

hList :: HLL input output => input -> HList output
hList = HList . toDynamicList

a :: HList '[]
a = hList ()

b :: HList '[Bool]
b = hList (OneTuple True)

c :: HList '[Bool, Int, Double, String]
c = hList (True, 24, 10.5, "Fire")

-- | just making sure the docs type check
main :: IO ()
main = do
  _ <- return (a,b,c)
  return ()
