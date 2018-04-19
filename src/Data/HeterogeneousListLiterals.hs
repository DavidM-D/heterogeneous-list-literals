{-# Language
  DataKinds,
  MultiParamTypeClasses,
  TypeInType,
  ConstraintKinds,
  FunctionalDependencies,
  FlexibleInstances
  #-}
module Data.HeterogeneousListLiterals (
    HeterogeneousListLiteral(..)
  , HLL
  , module Data.Tuple.Only
  ) where

import GHC.TypeLits
import Data.Kind
import Data.Tuple.Only
import Control.Monad
import Data.Dynamic
import Data.Tuple

-- | An alias for HeterogeneousListLiteral
type HLL = HeterogeneousListLiteral

-- | A type class which allows you to write tuples which can be transformed into a dynamic list.
--   The type of the input tuple is stored as a type level list in output, check the ReadMe for examples
class HeterogeneousListLiteral (input :: Type) (output :: [Type]) | input -> output, output -> input where
  toDynamicList :: input -> [Dynamic]

instance HeterogeneousListLiteral () '[] where
  toDynamicList () = []

instance Typeable a1 => HeterogeneousListLiteral (Only a1) '[a1] where
  toDynamicList (Only a1) = [toDyn a1]

-- all code generated below comes from this function
-- generate :: Int -- ^ up to N
--          -> String
-- generate n = unlines $ join $ map ("":) $ take n $ dropOneTuple res where
--   values = map ((:) 'a' . show) [1 :: Int ..]
--   dynamicList = map ("toDyn " ++) values
--   constraints = map ("Typeable " ++) values
--   withCommas = scanl1 (\a b -> a++","++b)
--   className = "HeterogeneousListLiteral"
--   template vals dynamicList constraint =
--       ["instance " ++ "(" ++ constraint ++ ") => " ++ className ++ " (" ++ vals ++ ") '[" ++ vals ++ "] where"
--     ,"  toDynamicList (" ++ vals ++ ") = [" ++ dynamicList ++ "]"]
--   res = zipWith3 template
--         (withCommas values)
--         (withCommas dynamicList)
--         (withCommas constraints)
--   dropOneTuple = tail

instance (Typeable a1,Typeable a2) => HeterogeneousListLiteral (a1,a2) '[a1,a2] where
  toDynamicList (a1,a2) = [toDyn a1,toDyn a2]

instance (Typeable a1,Typeable a2,Typeable a3) => HeterogeneousListLiteral (a1,a2,a3) '[a1,a2,a3] where
  toDynamicList (a1,a2,a3) = [toDyn a1,toDyn a2,toDyn a3]

instance (Typeable a1,Typeable a2,Typeable a3,Typeable a4) => HeterogeneousListLiteral (a1,a2,a3,a4) '[a1,a2,a3,a4] where
  toDynamicList (a1,a2,a3,a4) = [toDyn a1,toDyn a2,toDyn a3,toDyn a4]

instance (Typeable a1,Typeable a2,Typeable a3,Typeable a4,Typeable a5) => HeterogeneousListLiteral (a1,a2,a3,a4,a5) '[a1,a2,a3,a4,a5] where
  toDynamicList (a1,a2,a3,a4,a5) = [toDyn a1,toDyn a2,toDyn a3,toDyn a4,toDyn a5]

instance (Typeable a1,Typeable a2,Typeable a3,Typeable a4,Typeable a5,Typeable a6) => HeterogeneousListLiteral (a1,a2,a3,a4,a5,a6) '[a1,a2,a3,a4,a5,a6] where
  toDynamicList (a1,a2,a3,a4,a5,a6) = [toDyn a1,toDyn a2,toDyn a3,toDyn a4,toDyn a5,toDyn a6]

instance (Typeable a1,Typeable a2,Typeable a3,Typeable a4,Typeable a5,Typeable a6,Typeable a7) => HeterogeneousListLiteral (a1,a2,a3,a4,a5,a6,a7) '[a1,a2,a3,a4,a5,a6,a7] where
  toDynamicList (a1,a2,a3,a4,a5,a6,a7) = [toDyn a1,toDyn a2,toDyn a3,toDyn a4,toDyn a5,toDyn a6,toDyn a7]

instance (Typeable a1,Typeable a2,Typeable a3,Typeable a4,Typeable a5,Typeable a6,Typeable a7,Typeable a8) => HeterogeneousListLiteral (a1,a2,a3,a4,a5,a6,a7,a8) '[a1,a2,a3,a4,a5,a6,a7,a8] where
  toDynamicList (a1,a2,a3,a4,a5,a6,a7,a8) = [toDyn a1,toDyn a2,toDyn a3,toDyn a4,toDyn a5,toDyn a6,toDyn a7,toDyn a8]

instance (Typeable a1,Typeable a2,Typeable a3,Typeable a4,Typeable a5,Typeable a6,Typeable a7,Typeable a8,Typeable a9) => HeterogeneousListLiteral (a1,a2,a3,a4,a5,a6,a7,a8,a9) '[a1,a2,a3,a4,a5,a6,a7,a8,a9] where
  toDynamicList (a1,a2,a3,a4,a5,a6,a7,a8,a9) = [toDyn a1,toDyn a2,toDyn a3,toDyn a4,toDyn a5,toDyn a6,toDyn a7,toDyn a8,toDyn a9]

instance (Typeable a1,Typeable a2,Typeable a3,Typeable a4,Typeable a5,Typeable a6,Typeable a7,Typeable a8,Typeable a9,Typeable a10) => HeterogeneousListLiteral (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) '[a1,a2,a3,a4,a5,a6,a7,a8,a9,a10] where
  toDynamicList (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) = [toDyn a1,toDyn a2,toDyn a3,toDyn a4,toDyn a5,toDyn a6,toDyn a7,toDyn a8,toDyn a9,toDyn a10]

instance (Typeable a1,Typeable a2,Typeable a3,Typeable a4,Typeable a5,Typeable a6,Typeable a7,Typeable a8,Typeable a9,Typeable a10,Typeable a11) => HeterogeneousListLiteral (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11) '[a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11] where
  toDynamicList (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11) = [toDyn a1,toDyn a2,toDyn a3,toDyn a4,toDyn a5,toDyn a6,toDyn a7,toDyn a8,toDyn a9,toDyn a10,toDyn a11]

instance (Typeable a1,Typeable a2,Typeable a3,Typeable a4,Typeable a5,Typeable a6,Typeable a7,Typeable a8,Typeable a9,Typeable a10,Typeable a11,Typeable a12) => HeterogeneousListLiteral (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12) '[a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12] where
  toDynamicList (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12) = [toDyn a1,toDyn a2,toDyn a3,toDyn a4,toDyn a5,toDyn a6,toDyn a7,toDyn a8,toDyn a9,toDyn a10,toDyn a11,toDyn a12]

instance (Typeable a1,Typeable a2,Typeable a3,Typeable a4,Typeable a5,Typeable a6,Typeable a7,Typeable a8,Typeable a9,Typeable a10,Typeable a11,Typeable a12,Typeable a13) => HeterogeneousListLiteral (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13) '[a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13] where
  toDynamicList (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13) = [toDyn a1,toDyn a2,toDyn a3,toDyn a4,toDyn a5,toDyn a6,toDyn a7,toDyn a8,toDyn a9,toDyn a10,toDyn a11,toDyn a12,toDyn a13]

instance (Typeable a1,Typeable a2,Typeable a3,Typeable a4,Typeable a5,Typeable a6,Typeable a7,Typeable a8,Typeable a9,Typeable a10,Typeable a11,Typeable a12,Typeable a13,Typeable a14) => HeterogeneousListLiteral (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14) '[a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14] where
  toDynamicList (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14) = [toDyn a1,toDyn a2,toDyn a3,toDyn a4,toDyn a5,toDyn a6,toDyn a7,toDyn a8,toDyn a9,toDyn a10,toDyn a11,toDyn a12,toDyn a13,toDyn a14]

instance (Typeable a1,Typeable a2,Typeable a3,Typeable a4,Typeable a5,Typeable a6,Typeable a7,Typeable a8,Typeable a9,Typeable a10,Typeable a11,Typeable a12,Typeable a13,Typeable a14,Typeable a15) => HeterogeneousListLiteral (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15) '[a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15] where
  toDynamicList (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15) = [toDyn a1,toDyn a2,toDyn a3,toDyn a4,toDyn a5,toDyn a6,toDyn a7,toDyn a8,toDyn a9,toDyn a10,toDyn a11,toDyn a12,toDyn a13,toDyn a14,toDyn a15]

instance (Typeable a1,Typeable a2,Typeable a3,Typeable a4,Typeable a5,Typeable a6,Typeable a7,Typeable a8,Typeable a9,Typeable a10,Typeable a11,Typeable a12,Typeable a13,Typeable a14,Typeable a15,Typeable a16) => HeterogeneousListLiteral (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16) '[a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16] where
  toDynamicList (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16) = [toDyn a1,toDyn a2,toDyn a3,toDyn a4,toDyn a5,toDyn a6,toDyn a7,toDyn a8,toDyn a9,toDyn a10,toDyn a11,toDyn a12,toDyn a13,toDyn a14,toDyn a15,toDyn a16]

instance (Typeable a1,Typeable a2,Typeable a3,Typeable a4,Typeable a5,Typeable a6,Typeable a7,Typeable a8,Typeable a9,Typeable a10,Typeable a11,Typeable a12,Typeable a13,Typeable a14,Typeable a15,Typeable a16,Typeable a17) => HeterogeneousListLiteral (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17) '[a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17] where
  toDynamicList (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17) = [toDyn a1,toDyn a2,toDyn a3,toDyn a4,toDyn a5,toDyn a6,toDyn a7,toDyn a8,toDyn a9,toDyn a10,toDyn a11,toDyn a12,toDyn a13,toDyn a14,toDyn a15,toDyn a16,toDyn a17]

instance (Typeable a1,Typeable a2,Typeable a3,Typeable a4,Typeable a5,Typeable a6,Typeable a7,Typeable a8,Typeable a9,Typeable a10,Typeable a11,Typeable a12,Typeable a13,Typeable a14,Typeable a15,Typeable a16,Typeable a17,Typeable a18) => HeterogeneousListLiteral (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18) '[a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18] where
  toDynamicList (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18) = [toDyn a1,toDyn a2,toDyn a3,toDyn a4,toDyn a5,toDyn a6,toDyn a7,toDyn a8,toDyn a9,toDyn a10,toDyn a11,toDyn a12,toDyn a13,toDyn a14,toDyn a15,toDyn a16,toDyn a17,toDyn a18]

instance (Typeable a1,Typeable a2,Typeable a3,Typeable a4,Typeable a5,Typeable a6,Typeable a7,Typeable a8,Typeable a9,Typeable a10,Typeable a11,Typeable a12,Typeable a13,Typeable a14,Typeable a15,Typeable a16,Typeable a17,Typeable a18,Typeable a19) => HeterogeneousListLiteral (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19) '[a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19] where
  toDynamicList (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19) = [toDyn a1,toDyn a2,toDyn a3,toDyn a4,toDyn a5,toDyn a6,toDyn a7,toDyn a8,toDyn a9,toDyn a10,toDyn a11,toDyn a12,toDyn a13,toDyn a14,toDyn a15,toDyn a16,toDyn a17,toDyn a18,toDyn a19]

instance (Typeable a1,Typeable a2,Typeable a3,Typeable a4,Typeable a5,Typeable a6,Typeable a7,Typeable a8,Typeable a9,Typeable a10,Typeable a11,Typeable a12,Typeable a13,Typeable a14,Typeable a15,Typeable a16,Typeable a17,Typeable a18,Typeable a19,Typeable a20) => HeterogeneousListLiteral (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20) '[a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20] where
  toDynamicList (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20) = [toDyn a1,toDyn a2,toDyn a3,toDyn a4,toDyn a5,toDyn a6,toDyn a7,toDyn a8,toDyn a9,toDyn a10,toDyn a11,toDyn a12,toDyn a13,toDyn a14,toDyn a15,toDyn a16,toDyn a17,toDyn a18,toDyn a19,toDyn a20]
