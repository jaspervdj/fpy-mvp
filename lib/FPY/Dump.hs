-- | Small module to show things as a tree.  I wrote this for debugging while I
-- was offline on a plane so it's probably better to completely ignore it.
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
module FPY.Dump
    ( Dump (..)
    ) where

import           Data.Proxy
import           GHC.Generics
import           GHC.TypeLits
import qualified Data.Text as T

class Dump a where
    dump :: a -> [String]

    default dump :: (Generic a, GDump (Rep a)) => a -> [String]
    dump = gdump . from

instance (Dump a, Dump b) => Dump (a, b) where
    dump (l, r) = "(,)" : indent (dump l ++ dump r)

instance Dump a => Dump [a] where
    dump xs = "[]" : indent (concatMap dump xs)

instance Dump Integer where
    dump x = [show x]

instance Dump T.Text where
    dump t = [show t]

class GDump f where
    gdump :: f a -> [String]

instance GDump U1 where
    gdump _ = []

instance GDump a => GDump (D1 meta a) where
    gdump (M1 x) = gdump x

instance GDump a => GDump (S1 meta a) where
    gdump (M1 x) = gdump x

instance Dump a => GDump (K1 meta a) where
    gdump (K1 x) = dump x

instance forall a n f s. (GDump a, KnownSymbol n) =>
        GDump (C1 ('MetaCons n f s) a) where
    gdump (M1 x) = symbolVal (Proxy :: Proxy n) : indent (gdump x)

instance (GDump l, GDump r) => GDump (l :*: r) where
    gdump (l :*: r) = gdump l ++ gdump r

instance (GDump l, GDump r) => GDump (l :+: r) where
    gdump (L1 l) = gdump l
    gdump (R1 r) = gdump r

indent :: [String] -> [String]
indent = map (' ' :)
