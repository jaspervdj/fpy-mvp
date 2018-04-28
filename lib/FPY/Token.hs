{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module FPY.Token
    ( Formatting
    , FormattingToken (..)
    , parseFormatting

    , Formatted (..)
    , parseFormatted
    , Top (..)
    , parseTop
    , renderTop

    , Token (..)

    , List (..)
    , Tree (..)
    , _Number, _List
    , parseTree
    , renderTree
    ) where

import           Control.Applicative    ((<|>))
import           Control.Lens.TH
import           Control.Monad          (void)
import           Data.Monoid            ((<>))
import qualified Data.Text              as T
import qualified Data.Text.Lazy.Builder as TLB
import           FPY.Dump
import           GHC.Generics           (Generic)
import qualified Text.Parsec            as P

type Render a = a -> TLB.Builder

type Formatting = [FormattingToken]

data FormattingToken
    = Space
    | Tab
    | Newline
    | CarriageReturn
    | CComment T.Text
    deriving (Generic, Show)

instance Dump FormattingToken

parseFormatting :: P.Stream s m Char => P.ParsecT s u m Formatting
parseFormatting = P.many $
    (Space <$ P.char ' ')  <|>
    (Tab <$ P.char '\t') <|>
    (Newline <$ P.char '\n') <|>
    (CarriageReturn <$ P.char '\r') <|>
    (do
        void (P.string "/*")
        CComment . T.pack <$> P.manyTill P.anyChar (P.string "*/"))

renderFormatting :: Render Formatting
renderFormatting = foldMap go
  where
    go Space          = " "
    go Tab            = "\t"
    go Newline        = "\n"
    go CarriageReturn = "\r"
    go (CComment c)   = "/*" <> TLB.fromText c <> "*/"

-- | This represents an 'a' followed by some formatting.  That is how we usually
-- parse tokens.
data Formatted a = Formatted a Formatting
    deriving (Foldable, Functor, Generic, Show, Traversable)

instance Dump a => Dump (Formatted a)

parseFormatted
    :: P.Stream s m Char => P.ParsecT s u m a -> P.ParsecT s u m (Formatted a)
parseFormatted p = Formatted <$> p <*> parseFormatting

renderFormatted :: Render a -> Render (Formatted a)
renderFormatted r (Formatted x f) = r x <> renderFormatting f

-- | In general, we always parse tokens and then the trailing formatting
-- (whitespace, comments, etc.).  This works well except for the very beginning
-- of the file, where we can also have some formatting.  This datatype
-- represents this top-level entry point.
data Top a = Top Formatting (Formatted a)
    deriving (Foldable, Functor, Generic, Show, Traversable)

instance Dump a => Dump (Top a)

parseTop
    :: P.Stream s m Char => P.ParsecT s u m a -> P.ParsecT s u m (Top a)
parseTop p = Top <$> parseFormatting <*> parseFormatted p <* P.eof

renderTop :: Render a -> Render (Top a)
renderTop r (Top f x) = renderFormatting f <> renderFormatted r x

data Token
    = LBracket
    | RBracket
    | Comma
    deriving (Generic, Show)

instance Dump Token

parseLBracket :: P.Stream s m Char => P.ParsecT s u m Token
parseLBracket = LBracket <$ P.char '['

parseRBracket :: P.Stream s m Char => P.ParsecT s u m Token
parseRBracket = RBracket <$ P.char ']'

parseComma :: P.Stream s m Char => P.ParsecT s u m Token
parseComma = Comma <$ P.char ','

renderToken :: Render Token
renderToken LBracket = "["
renderToken RBracket = "]"
renderToken Comma    = ","

-- Note that numbers can be formatted in different ways, e.g. "1e1" and "10.0".
-- This code currently does not take that into account.
parseNumber
    :: P.Stream s m Char => P.ParsecT s u m Integer
parseNumber = read <$> P.many1 P.digit

renderNumber :: Render Integer
renderNumber = TLB.fromString . show

data List a
    -- The last 'Token' that closes the list is not formatted.  This way, the
    -- comments belonging to it get attached to the 'Tree' as well, which is
    -- better, since it allows us to replace the deeply nested bits without the
    -- comments disappearing from the tree.
    = EmptyBracketList (Formatted Token) Token
    | BracketList (Formatted Token) a [(Formatted Token, a)] Token
    deriving (Generic, Show)

instance Dump a => Dump (List a)

parseFormattedList
    :: P.Stream s m Char
    => P.ParsecT s u m a
    -> P.ParsecT s u m (List a)
parseFormattedList item =
    parseFormatted parseLBracket >>= \open ->
    (do
        item0 <- item
        items <- P.many $ (,) <$> parseFormatted parseComma <*> item
        close <- parseRBracket
        return $ BracketList open item0 items close) <|>
    (do
        close <- parseRBracket
        return $ EmptyBracketList open close)

renderFormattedList :: Render a -> Render (List a)
renderFormattedList _rx (EmptyBracketList open close) =
    renderFormatted renderToken open <> renderToken close
renderFormattedList rx (BracketList open item0 items close) =
    renderFormatted renderToken open <> rx item0 <>
    mconcat [renderFormatted renderToken t <> rx x | (t, x) <- items] <>
    renderToken close

data Tree
    = Number Integer
    | List (List (Formatted Tree))
    deriving (Generic, Show)

instance Dump Tree

parseTree :: P.Stream s m Char => P.ParsecT s u m Tree
parseTree =
    (Number <$> parseNumber) <|>
    (List <$> parseFormattedList (parseFormatted parseTree))

renderTree :: Render Tree
renderTree (Number n) = renderNumber n
renderTree (List l)   = renderFormattedList (renderFormatted renderTree) l

$(makePrisms ''Tree)
