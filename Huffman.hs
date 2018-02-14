--------------------------------------------------------------------------------
-- | Program that encodes and decodes Huffmancodes.
-- 
-- Copyright  : (c) Johan Öhlund, 2018
-- Maintainer : c15jod@cs.umu.se
--
-- Project for the course Programspråk VT18, Umeå universitet.
--------------------------------------------------------------------------------
module Huffman (Htree,encode,decode) where

import Data.List
import Data.Function
import Data.Tree
import Data.Char

data Htree = Leaf Char | Branch Htree Htree deriving(Eq, Ord,Read)
data Wtree = L Integer Char | B Integer Wtree Wtree deriving(Eq, Ord,Read)

{-
Instances: Show instances for Htree and Wtree, function drawTree is imported 
            from Data.Tree to print the trees nicely.
Comment: Prints Wtree and Htree nicely.
-}
instance Show Wtree where
  show (L i c) = show i ++ " " ++ show c 
  show (B i h1 h2) = "\n"++(drawTree $ wtreeToTree (B i h1 h2))

instance Show Htree where
  show (Leaf c) = show c 
  show (Branch h1 h2) = "\n"++(drawTree $ htreeToTree (Branch h1 h2))

{-
Function: htreeToTree
Comment: Converts a Htree to a Tree used for Show instance.
-}
htreeToTree :: Htree -> Tree String
htreeToTree (Leaf c) = Node (show c) []
htreeToTree (Branch h1 h2) = Node "B*" [htreeToTree h1 , htreeToTree h2]

{-
Function: wtreeToTree
Comment: Converts a Wtree to a Tree used for Show instance.
-}
wtreeToTree :: Wtree -> Tree String
wtreeToTree (L i c) = Node ((show i)++" "++(show c)) []
wtreeToTree (B i h1 h2) = Node ((show i)++"B*") 
                            [wtreeToTree h1,wtreeToTree h2]

{-
Deluppgift 1:
Function: statistics
Comment: Counts the number of instances for each character in a string.
-}
statistics :: String -> [(Integer, Char)] 
statistics [] = []
statistics (x:xs) = countLetters (x:xs) : statistics [res| res<-xs, res/=x] 

{-
Function: countLetters
Comment: Checks the first letter in the String and returns the char and the
        number of occurrences of that letter as a tuple.
-}
countLetters :: String -> (Integer,Char)
countLetters (x:xs) = (sum [1|c<-(x:xs),c==x],x)

{-
Deluppgift 2:
Function: maketree
Comment: Creates an optimal Huffmantree step by step, creating Wtrees, sorting
        Wtrees, forming one Wtree with the list of Wtrees and finally creating 
        the Htree. 
-}
maketree :: [(Integer, Char)] -> Htree
maketree x = (createHtree)$(addWtree)$(sort)$(makeWtreeList x)  

{-
Function: createHtree
Comment: Converts the final Wtree into a Htree.
-}
createHtree :: Wtree -> Htree
createHtree (L i1 w1)       = (Leaf w1)
createHtree (B i1 w1 w2)    = (Branch ((createHtree) w1) ((createHtree) w2))

{-
Function: makeWtreeList
Comment: Converts the list of letters and thier weights (ouput from function 
        statistics) and create a list of Wtrees.
-}
makeWtreeList :: [(Integer, Char)] -> [Wtree]
makeWtreeList ((i,c):[]) = [L i c] 
makeWtreeList ((i,c):xs) = L i c : makeWtreeList xs

{-
Function: getWtreeWeight
Comment: Returns the weight of a Wtree.
-}
getWtreeWeight :: Wtree -> Integer
getWtreeWeight (L i _)      = i
getWtreeWeight (B i _ _)    = i 

{-
Function: addWtree
Comment: Creates branches (B) of leafs (L) and/or other branches (B) by
        adding their weights and form a new branch (B).
-}
addWtree :: [Wtree] -> Wtree
addWtree (w1:[]) = w1
addWtree (w1:w2:xs) = (addWtree) $(sort)(((addWtree')
                        (getWtreeWeight w1)(getWtreeWeight w2) w1 w2):xs)

{-
Function: addWtree'
Comment: Help function for addWtree that creates new branch (B) by 
        adding the subtrees weights to the new branch (B).
-}
addWtree' :: Integer -> Integer -> Wtree -> Wtree -> Wtree
addWtree' i1 i2 w1 w2= (B (i1+i2) w1 w2)

{-
Deluppgift 3:
Function: encode
Comment: Encodes a given string using HuffmanCoding. Returns the created
        Huffmantree and the Huffmancode for the String.
-}
encode :: String -> (Htree,[Integer])
encode []   = error "Can not encode empty String..."
encode str  = convertToOuput str ((maketree) $(statistics) str)


{-
Function: convertToOuput
Comment: Converts to the expected output of encode.
-}
convertToOuput :: String -> Htree -> (Htree,[Integer])
convertToOuput str htree  = (htree,(encode') str htree) 

{-
Function: encode'
Comment: Help function to encode. Handels the specialcase if the Htree only
         got a single letter.
-}
encode' :: String -> Htree -> [Integer]
encode' [] htree = []
encode' (x:xs) (Leaf c)    = take (sum [1|a<-(x:xs),a==c]) (repeat 0)
encode' (x:xs) htree        = ((traverseDF) htree x []) ++ encode' xs htree

{-
Function: traverseDF
Comment: Uses "Depth first search" to find the Huffman code of a letter. 
-}
traverseDF :: Htree -> Char-> [Integer] -> [Integer]
traverseDF (Leaf c1) c output    = if c1==c then reverse output else []
traverseDF (Branch l r) c output = (traverseDF l c (0:output)) 
                                        ++ (traverseDF r c (1:output))

{-
Deluppgift 4:
Function: decode
Comment: Decodes a Huffmancode, given the huffmancode and the Htree.
-}
decode :: Htree -> [Integer] -> String
decode _ []             = error "Can not decode empty Huffmancode..."
decode (Leaf c1) hcode  = take (length hcode) (repeat c1)
decode htree x          = decode' htree htree x

{-
Function: decode'
Comment: Help function for decode.
-}
decode' :: Htree -> Htree ->[Integer] -> String
decode' _ (Branch _ _) []           = error "Huffmancode stopped on a Branch."
decode' _ (Leaf c1) []              = [c1]
decode' htree (Leaf c1) (x:xs)      = c1: decode' htree htree (x:xs)
decode' htree (Branch l r) (x:xs)   = if x == 1
                                        then decode' htree r xs 
                                        else decode' htree l xs
