{-# LANGUAGE OverloadedStrings #-}

{- |
    Parsing text in the Enamex data format.  Each node is enclosed between
    opening and closing tags with tag name representing the label and contents
    representing children of the node.  Both leaf and label values should be
    escaped by prepending the \\ character before special >, <, \\ and space
    characters.

    Example:

>>> :m Text.Named.Enamex Data.Named.Tree Data.Text.Lazy
>>> let drawIt = putStr . drawForest . mapForest show . parseForest
>>> drawIt $ pack "<x>w1.1\\ w1.2</x> <y><z>w2</z> w3</y>"
Left "x"
|
`- Right "w1.1 w1.2"
,
Left "y"
|
+- Left "z"
|  |
|  `- Right "w2"
|
`- Right "w3"
-}

module Text.Named.Enamex
(
-- * Parsing
  parseForest
, parseEnamex

-- * Printing
, showForest
, showEnamex
) where

import           Control.Applicative
import           Control.Monad             (when, (<=<))
-- import Data.Monoid
import           Data.Attoparsec.Text.Lazy
import           Data.Function             (on)
import           Data.List                 (intersperse)
import qualified Data.Named.Tree           as Tr
import qualified Data.Text                 as T
import qualified Data.Text.Lazy            as L
import qualified Data.Text.Lazy.Builder    as L

pForest :: Parser (Tr.NeForest T.Text T.Text)
pForest = pTree `sepBy` (space *> skipSpace)

pTree :: Parser (Tr.NeTree T.Text T.Text)
pTree = pNode
    <|> pLeaf

pLeaf :: Parser (Tr.NeTree T.Text T.Text)
pLeaf = Tr.Node <$> (Right <$> pWord) <*> pure []

pNode :: Parser (Tr.NeTree T.Text T.Text)
pNode = do
    x    <- pOpenTag
    kids <- pForest
    x'   <- pCloseTag
    when (x /= x') (fail "Tag start/end mismatch")
    return $ Tr.Node (Left x) kids

pOpenTag :: Parser T.Text
pOpenTag = "<" *> pWord <* ">"

pCloseTag :: Parser T.Text
pCloseTag = "</" *> pWord <* ">"

pWord :: Parser T.Text
pWord =
    unEscape <$> scan False special
  where
    special False c =
      case c == ' ' || c == '<' || c == '>' of
        True    -> Nothing
        False   -> if c == '\\'
            then Just True
            else Just False
    special True _  = Just False

-- | TODO: Use lazy text builder to avoid slowness in the pessimistic case.
unEscape :: T.Text -> T.Text
unEscape xs = x `T.append` case drop1 rest of
    Just (y, ys) -> y `T.cons` unEscape ys
    Nothing      -> ""
  where
    drop1 = T.uncons <=< return . snd <=< T.uncons
    (x, rest) = T.breakOn "\\" xs

-- | TODO: Use lazy text builder to avoid slowness in the pessimistic case.
escape :: T.Text -> T.Text
escape x = case T.uncons z of
    Nothing     -> y
    Just (c, q) -> y
        `T.append` ('\\'
        `T.cons` (c
        `T.cons` escape q))
  where
    (y, z) = T.break special x
    special c = c == ' ' || c == '<' || c == '>' || c == '\\'

-- | Parse the enamex forest.
parseForest :: L.Text -> Tr.NeForest T.Text T.Text
parseForest = either error id . eitherResult . parse (pForest <* endOfInput)

-- | Parse the enamex file.
parseEnamex :: L.Text -> [Tr.NeForest T.Text T.Text]
parseEnamex = map parseForest . L.lines

data Tag = Open | Close | Body

-- | Function which determines between which two tag elements a space
-- character should be inserted.
noSpace :: Tag -> Tag -> Bool
noSpace Open  _     = True
noSpace Body  Close = True
noSpace Close Close = True
noSpace _     _     = False

-- | We define our own groupBy because the standard version from Data.List
-- assumes that the predicate is transitive.
groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy p (x : y : xs)
    | p x y     = join x $ groupBy p (y : xs)
    | otherwise = [x]    : groupBy p (y : xs)
  where
    join z (zs : zss) = (z : zs) : zss
    join z [] = [[z]]
groupBy _ [x] = [[x]]
groupBy _ []  = []

buildForest :: Tr.NeForest t t -> [(t, Tag)]
buildForest = concat . map buildTree

buildTree :: Tr.NeTree t t -> [(t, Tag)]
buildTree (Tr.Node (Left x) ts) = (x, Open) : buildForest ts ++ [(x, Close)]
buildTree (Tr.Node (Right x) _) = [(x, Body)]

buildStream :: [(T.Text, Tag)] -> L.Builder
buildStream
    = mconcat . intersperse " "
    . map (mconcat . map buildTag)
    . groupBy (noSpace `on` snd)

buildTag :: (T.Text, Tag) -> L.Builder
buildTag (x, tag) = case tag of
    Open    -> "<"  `mappend` y `mappend` ">"
    Close   -> "</" `mappend` y `mappend` ">"
    _       -> y
  where
    y = L.fromText (escape x)

-- | Show the forest.
showForest :: Tr.NeForest T.Text T.Text -> L.Text
showForest = L.toLazyText . buildStream . buildForest

-- | Show the enamex file.
showEnamex :: [Tr.NeForest T.Text T.Text] -> L.Text
showEnamex = L.toLazyText . mconcat . map (L.fromLazyText . showForest)
