--------------------------------------------------------------------------------
-- | Program that encodes and decodes Huffmancodes.
-- 
-- Copyright  : (c) Johan Öhlund, 2018
-- Maintainer : c15jod@cs.umu.se
--
-- Project for the course Programspråk VT18, Umeå universitet.
--------------------------------------------------------------------------------

import Data.List
import Data.Function

data Htree = Leaf Char | Branch Htree Htree deriving(Eq, Ord)
data Wtree = L Integer Char | B Integer Wtree Wtree deriving(Eq, Ord)


--Show instance for Wtree.
--If an expression is surrounded by "{...}" then it belongs to a B. 
instance Show Wtree where
    show wtree = "\n"++showWtree wtree 0 ++ "\n"

--Show instance for Htree.
--If an expression is surrounded by "{...}" then it belongs to a Branch.
instance Show Htree where
    show htree = "\n"++showHtree htree 0 ++ "\n"
 

{-
Function: padding
Comment: Used for padding for Show instances of Htree and Wtree.
-}
padding :: Integer -> String
padding 0 = ""
padding n = "." ++ padding(n-1)

{-
Function: showWtree
Comment: Displays the Wtree nicely.
-}
showWtree :: Wtree -> Integer -> String
showWtree (L i c) n = (padding n) ++ "L "++show i ++ " " ++show c
showWtree (B i l r) n = let showl = showWtree l (n+4) in
                           let showr = showWtree r (n+4) in
                           let showc = (padding n) ++ "B "++show i++"{" in
                           showc ++ "\n" ++ showl ++ "\n" ++ showr ++ "\n" 
                            ++ (padding n) ++ "}"

{-
Function: showHtree
Comment: Displays the Htree nicely.
-}
showHtree :: Htree -> Integer -> String
showHtree (Leaf c) n = (padding n) ++ "Leaf "++show c
showHtree (Branch l r) n = let showl = showHtree l (n+4) in
                           let showr = showHtree r (n+4) in
                           let showc = (padding n) ++ "Branch{" in
                           showc ++ "\n" ++ showl ++ "\n" ++ showr ++ "\n" 
                            ++ (padding n) ++ "}"



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
maketree x = (createHtree)$(createOneWtree)$(sortWtree)$(makeWtree x)  

{-
Function: createHtree
Comment: Converts the final Wtree into a Htree.
-}
createHtree :: Wtree -> Htree
createHtree (L i1 w1)       = (Leaf w1)
createHtree (B i1 w1 w2)    = (Branch ((createHtree) w1) ((createHtree) w2))

{-
Function: makeWtree
Comment: Converts the list of letters and thier weights (ouput from function 
        statistics) and create a list of Wtrees.
-}
makeWtree :: [(Integer, Char)] -> [Wtree]
makeWtree ((i,c):[]) = [L i c] 
makeWtree ((i,c):xs) = L i c : makeWtree xs

{-
Function: sortWtree
Comment: Sorts the Wtree by their weight.
-}
sortWtree :: [Wtree] -> [Wtree]
sortWtree = sortBy (compare `on` (getWtreeWeight))

{-
Function: getWtreeWeight
Comment: Returns the weight of a Wtree.
-}
getWtreeWeight :: Wtree -> Integer
getWtreeWeight (L i _)      = i
getWtreeWeight (B i _ _)    = i 

{-
Function: createOneWtree
Comment: Uses recursion to construct a single Wtree from a list of Wtrees. 
-}
createOneWtree :: [Wtree] -> Wtree  
createOneWtree wtreeList
    | length wtreeList == 1 = head wtreeList
    | otherwise             = createOneWtree $(addWtree wtreeList)

{-
Function: addWtree
Comment: Creates branches (B) of leafs (L) and/or other branches (B) by
        adding their weights and form a new branch (B).
-}
addWtree :: [Wtree] -> [Wtree]
addWtree ((L i0 c0)   :(L i1 c1)   :xs)= 
                            sortWtree ((B (i0+i1) (L i0 c0)    (L i1 c1))   :xs) 
addWtree ((B i0 w1 w2):(L i1 c1)   :xs)= 
                            sortWtree ((B (i0+i1) (B i0 w1 w2) (L i1 c1))   :xs)
addWtree ((L i0 c0)   :(B i1 w1 w2):xs)= 
                            sortWtree ((B (i0+i1) (L i0 c0)    (B i1 w1 w2)):xs)
addWtree ((B i0 w1 w2):(B i1 w3 w4):xs)= 
                            sortWtree ((B (i0+i1) (B i0 w1 w2) (B i1 w3 w4)):xs)


{-
Deluppgift 3:
Function: encode
Comment: Encodes a given string using HuffmanCoding. Returns the created
        Huffmantree and the Huffmancode for the String.
-}
encode :: String -> (Htree,[Integer])
encode []   = error "Can not encode empty String..."
encode str  = convertToOuput str $(maketree) $(statistics) str


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
encode' (x:xs) (Leaf c1)    = specialCaseEncode x (x:xs)
encode' (x:xs) htree        = ((traverseDF) htree x []) ++ encode' xs htree

{-
Function: specialCaseEncode
Comment: If only one letter is in the encoding string then we encode it with 
            zeros.
-}
specialCaseEncode :: Char -> [Char] -> [Integer]
specialCaseEncode c str = take (sum [1|x<-str,x==c]) (repeat 0)

{-
Function: specialCaseDecode
Comment: If only one letter is in the decode string then we decode it with 
            a special case.
-}
specialCaseDecode :: Char -> [Integer] -> [Char]
specialCaseDecode c hcode = take (length hcode) (repeat c)

{-
Function: traverseDF
Comment: Uses "Depth first search" to find the letter. 
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
decode (Leaf c1) hcode  = specialCaseDecode c1 hcode
decode htree x          = decode' htree htree x

{-
Function: decode'
Comment: Help function for decode.
-}
decode' :: Htree -> Htree ->[Integer] -> String
decode' _ (Leaf c1) []              = [c1]
decode' htree (Leaf c1) (x:xs)      = c1: decode' htree htree (x:xs)
decode' htree (Branch l r) (x:xs)   = if x == 1
                                        then decode' htree r xs 
                                        else decode' htree l xs
{-
let x = encode "0123456789 abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ !?*.+"
let y = decode (fst x) (snd x)
https://www.siggraph.org/education/materials/HyperGraph/video/mpeg/mpegfaq/huffman_tutorial.html
-}