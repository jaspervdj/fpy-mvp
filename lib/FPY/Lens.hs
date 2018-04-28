{-# LANGUAGE Rank2Types   #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module FPY.Lens
    ( nth
    , number
    ) where

import           Control.Lens
import           FPY.Token

class TraverseTree a where
    traverseTree :: Traversal' a Tree

instance TraverseTree Tree where
    traverseTree = ($)

instance TraverseTree a => TraverseTree (Top a) where
    traverseTree f = traverse (traverseTree f)

instance TraverseTree a => TraverseTree (Formatted a) where
    traverseTree f = traverse (traverseTree f)

type instance Index (List a) = Int

type instance IxValue (List a) = a

instance Ixed (List a) where
    ix _ _ (EmptyBracketList open close) =
        pure (EmptyBracketList open close)

    ix 0 f (BracketList open item0 items close) =
        BracketList open <$> f item0 <*> pure items <*> pure close

    ix n f (BracketList open item0 items close) =
        BracketList open item0 <$> ix (n - 1) (_2 f) items <*> pure close

nth :: TraverseTree a => Int -> Traversal' a (Formatted Tree)
nth n = traverseTree . _List . ix n

number :: TraverseTree a => Traversal' a Integer
number = traverseTree . _Number
