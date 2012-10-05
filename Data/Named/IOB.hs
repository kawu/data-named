{- |
    IOB encoding method extended to forests.

    Example:

>>> :m Data.Tree Data.Text.Lazy Text.Named.Enamex
>>> :m + Data.Named.IOB Data.Named.Tree
>>> let enamex = pack "<x>w1.1\\ w1.2</x> w2 <y><z>w3</z> w4</y>"

>>> putStr . drawForest . mapForest show . parseForest $ enamex
Left "x"
|
`- Right "w1.1 w1.2"
,
Right "w2"
,
Left "y"
|
+- Left "z"
|  |
|  `- Right "w3"
|
`- Right "w4"

>>> mapM_ print . encodeForest . parseForest $ enamex
IOB {word = "w1.1 w1.2", label = [B "x"]}
IOB {word = "w2", label = []}
IOB {word = "w3", label = [B "y",B "z"]}
IOB {word = "w4", label = [I "y"]}
-}

module Data.Named.IOB
( IOB (..)
, Label
, Atom (..)
, encodeForest
, decodeForest
) where

import Control.Applicative ((<$>))
import Data.Maybe (fromJust)
import Data.Tree

-- | An 'IOB' data structure consists of a word with a corresponding
-- compound label.
data IOB w a = IOB
    { word  :: w
    , label :: Label a
    } deriving (Show)

-- | A 'Label' consists of a list of atomic 'Atom' labels.
type Label a = [Atom a]

-- | An 'Atom' is the atomic label with additional marker.
data Atom a  = B a      -- ^ Beginning marker
             | I a      -- ^ Inside marker 
             deriving (Show, Eq, Ord)

push :: Atom a -> IOB w a -> IOB w a
push x (IOB w xs) = IOB w (x:xs)

popMaybe :: IOB w a -> Maybe (Atom a, IOB w a)
popMaybe (IOB w (x:xs)) = Just (x, IOB w xs)
popMaybe (IOB _ [])     = Nothing

topMaybe :: IOB w a -> Maybe (Atom a)
topMaybe iob = fst <$> popMaybe iob

pop :: IOB w a -> (Atom a, IOB w a)
pop = fromJust . popMaybe

-- top :: IOB w a -> Atom a
-- top = fromJust . topMaybe

raw :: Atom a -> a
raw (B x) = x
raw (I x) = x

-- isB :: Atom a -> Bool
-- isB (B _) = True
-- isB _     = False

isI :: Atom a -> Bool
isI (I _) = True
isI _     = False

-- | Encode the forest with the IOB method.
encodeForest :: Forest (Either a w) -> [IOB w a]
encodeForest [] = []
encodeForest (x:xs) = encodeTree x ++ encodeForest xs

-- | Encode the tree using the IOB method.
encodeTree :: Tree (Either a w) -> [IOB w a]

encodeTree (Node (Left _) []) =
    error "encodeTree: label node with no children"
encodeTree (Node (Left e) forest) =
    let addLayer (x:xs) = push (B e) x : map (push $ I e) xs
        addLayer []     = []
    in  addLayer (encodeForest forest)

encodeTree (Node (Right _) (_:_)) =
    error "encodeTree: word node with children"
encodeTree (Node (Right w) _) = [IOB w []]

-- | Decode the forest using the IOB method.
decodeForest :: Eq a => [IOB w a] -> Forest (Either a w)
decodeForest [] = []
decodeForest xs =
    tree : decodeForest xs'
  where
    (chunk, xs') = followTop xs
    tree = case topMaybe $ head chunk of
        Nothing -> Node (Right . word $ head chunk) []
        Just e  -> Node (Left $ raw e) (decodeForest $ map rmTop chunk)
    rmTop = snd . pop

-- | Take iob elements as long as the top label doesn't change.  
-- Return obtained part together with the rest of iob.
followTop :: Eq a => [IOB w a] -> ([IOB w a], [IOB w a])
followTop [] = error "followTop: empty iob"
followTop (x:xs) =
    (x:chunk, rest)
  where
    (chunk, rest) = span (cond (topMaybe x) . topMaybe) xs
    cond (Just a) (Just b) = raw a == raw b && isI b
    cond _ _ = False
