import Data.List
import Data.Function
data Htree = Leaf Char | Branch Htree Htree deriving(Show)
data Wtree = L Integer Char | B Integer Wtree Wtree deriving(Show)
{-
Deluppgift 1:
-}

{-
Function: statistics
Comment: Counts the number of instances for each character in a string.
-}
statistics :: String -> [(Integer, Char)] 
statistics [] = []
statistics (x:xs) = countLetters (x:xs) : statistics [res| res<-xs, res/=x] 

countLetters :: String -> (Integer,Char)
countLetters (x:xs) = (sum [1|c<-(x:xs),c==x],x)

{-
Deluppgift 2
-}
maketree :: [(Integer, Char)] -> Htree
maketree x = (checkDistinct)$(createHtree)$(createOneWtree)$(sortWtree)$(makeWtree x)  

createHtree :: Wtree -> Htree
createHtree (L i1 w1)  = (Leaf w1)
createHtree (B i1 w1 w2) = (Branch ((createHtree) w1) ((createHtree) w2))

makeWtree :: [(Integer, Char)] -> [Wtree]
makeWtree ((i,c):[]) = [L i c] 
makeWtree ((i,c):xs) = L i c : makeWtree xs

sortWtree :: [Wtree] -> [Wtree]
--sortWtree [] =  []
sortWtree = sortBy (compare `on` (test))

test :: Wtree -> Integer
test (L i c) = i
test (B i w1 w2) = i 

checkDistinct :: Htree -> Htree
checkDistinct (Leaf c) = Branch (Leaf c) (Leaf c)
checkDistinct (Branch w1 w2) = Branch w1 w2

--test :: Htree -> Htree
--test (Leaf c) = (Leaf c):(Leaf c):[]

--increasing' :: Wtree -> Wtree-> Ordering
--increasing' (L x _) (L y _)    = compare x y
--increasing' (B x _ _) (L y _)  = compare x y
--increasing' (L x _) (B y _ _)  = compare x y
--increasing' (B x _ _) (B y _ _)= compare x y


createOneWtree :: [Wtree] -> Wtree  
createOneWtree x
    | length x == 1 = head x
    | otherwise = createOneWtree $(addWtree x)


addWtree :: [Wtree] -> [Wtree]
addWtree ((L i1 c1):(L i2 c2):xs)    = sortWtree ((B (i1+i2) (L i1 c1) (L i2 c2)):xs) 
addWtree ((B i0 a b):(L i3 c3):xs)   = sortWtree ((B (i0+i3) (B i0 a b) (L i3 c3)):xs)
addWtree ((L i3 c3):(B i0 a b):xs)   = sortWtree ((B (i0+i3) (B i0 a b) (L i3 c3)):xs)
addWtree ((B i0 a b):(B i3 a2 b2):xs)= sortWtree ((B (i0+i3) (B i0 a b) (B i3 a2 b2)):xs)



encode :: String -> (Htree,[Integer])
encode x = (htree,(encode') x htree) 
        where htree = (maketree) $(statistics) x

encode' :: String -> Htree -> [Integer]
encode' [] htree = []
encode' (x:xs) (Branch (Leaf c1) (Leaf c2)) = 
        if c1==c2 
            then hardCode c1 (x:xs) 
        else ((traverseDF) (Branch (Leaf c1) (Leaf c2)) x []) 
                ++ encode' xs (Branch (Leaf c1) (Leaf c2))

encode' (x:xs) htree = ((traverseDF) htree x []) ++ encode' xs htree

hardCode :: Char -> [Char] -> [Integer]
hardCode c str = take (sum [1|x<-str,x==c]) (repeat 0)


traverseDF :: Htree -> Char-> [Integer] -> [Integer]
traverseDF (Leaf c1) c output    = if c1==c then reverse output else []
traverseDF (Branch l r) c output = (traverseDF l c (0:output)) 
                ++ (traverseDF r c (1:output))

decode :: Htree -> [Integer] -> String
decode _ [] = []
decode htree x = decode' htree htree x

decode' :: Htree -> Htree ->[Integer] -> String
decode' _ (Leaf c1) [] = [c1]
decode' htree (Leaf c1) (x:xs) = c1: decode' htree htree (x:xs)
decode' htree (Branch l r) (x:xs) = if x == 1
                                        then decode' htree r xs 
                                        else decode' htree l xs
{-


let x = statistics "huffman"
let y = makeWtree x
let z = sortWtree y
let w = createOneWtree z
createHtree w

2:

encode "aaaa"
traverseDF y

DECODE:
let x = encode "aaaaa fgdg"
let y = fst x
let z = snd x 
decode y z

https://www.siggraph.org/education/materials/HyperGraph/video/mpeg/mpegfaq/huffman_tutorial.html
-}