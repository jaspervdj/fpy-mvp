module FPY
    ( module FPY.Token
    , module FPY.Lens
    , parse
    ) where

import qualified Data.Text   as T
import           FPY.Lens
import           FPY.Token
import qualified Text.Parsec as P

parse :: T.Text -> Either P.ParseError (Top Tree)
parse input = P.parse (parseTop parseTree) "-" input
