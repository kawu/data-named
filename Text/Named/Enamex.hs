{-# LANGUAGE OverloadedStrings #-}

module Text.Named.Enamex
( parseForest
, parseEnamex
, mapTwo
) where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Text
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Tree as Tree

type Tree = Tree.Tree T.Text
type Forest = Tree.Forest T.Text

-- | Map the first function over internal nodes
-- and the second one over leaves.
mapTwo :: (T.Text -> a) -> (T.Text -> b)
         -> Tree.Tree T.Text -> Tree.Tree (Either a b)
mapTwo _ g (Tree.Node x [])   = Tree.Node (Right $ g x) []
mapTwo f g (Tree.Node x kids) = Tree.Node (Left $ f x) (map (mapTwo f g) kids)

pForest :: Parser Forest
pForest = pTree `sepBy` (space *> skipSpace)

pTree :: Parser Tree
pTree = pNode
    <|> pLeaf

pLeaf :: Parser Tree
pLeaf = Tree.Node <$> pWord <*> pure []

pNode :: Parser Tree
pNode = do
    x    <- pOpenTag
    kids <- pForest
    x'   <- pCloseTag
    when (x /= x') (fail "Tag start/end mismatch")
    return $ Tree.Node x kids

pOpenTag :: Parser T.Text
pOpenTag = "<" .*> pWord <*. ">"

pCloseTag :: Parser T.Text
pCloseTag = "</" .*> pWord <*. ">"

pWord :: Parser T.Text
pWord =
    scan False special
  where
    special False c =
      case c == ' ' || c == '<' || c == '>' of
        True    -> Nothing
        False   -> if c == '\\'
            then Just True
            else Just False
    special True _  = Just False

-- | Parse the enamex forest.
parseForest :: T.Text -> Forest
parseForest = either error id . parseOnly (pForest <* endOfInput)

-- | Parse the enamex file.
parseEnamex :: L.Text -> [Forest]
parseEnamex = map (parseForest . L.toStrict) . L.lines
