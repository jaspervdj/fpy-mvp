module Main where

import           Control.Lens
import qualified Data.Text.IO           as T
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.IO      as TL
import qualified FPY
import qualified FPY.Dump

main :: IO ()
main = do
    contents <- T.getContents
    case FPY.parse contents of
        Left err -> fail $ show err
        Right x  -> do
            putStrLn $ unlines $ FPY.Dump.dump x
            TL.putStr $ TLB.toLazyText $ FPY.renderTop FPY.renderTree $ x
                -- Turn the second element in the list into "20".
                & FPY.nth 1 . traverse .~ FPY.Number 20
                -- Add one to the fifth element in the list.
                & FPY.nth 4 . FPY.number %~ (+ 1)
