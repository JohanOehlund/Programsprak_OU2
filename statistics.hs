import Data.List

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
maketree x = (createHtree)$(createWtree)$(sortWtree)$(makeWtree x)  

createHtree :: Wtree -> Htree
createHtree (L i1 w1)  = (Leaf w1)
createHtree (B i1 w1 w2) = (Branch ((createHtree) w1) ((createHtree) w2))

makeWtree :: [(Integer, Char)] -> [Wtree]
makeWtree ((i,c):[]) = [L i c]
makeWtree ((i,c):xs) = L i c : makeWtree xs

sortWtree :: [Wtree] -> [Wtree]
sortWtree [] = []
sortWtree (x:xs) = sortBy (increasing') (x:xs)

increasing' :: Wtree -> Wtree-> Ordering
increasing' (L x _) (L y _)    = compare x y
increasing' (B x _ _) (L y _)  = compare x y
increasing' (L y _) (B x _ _)  = compare x y
increasing' (B x _ _) (B y _ _)= compare x y


createWtree :: [Wtree] -> Wtree  
createWtree x
    | length x == 1 = head x
    | otherwise = createWtree $(addWtree x)


addWtree :: [Wtree] -> [Wtree]
addWtree ((L i1 c1):(L i2 c2):xs) = sortBy (increasing') 
            (B (i1+i2) (L i1 c1) (L i2 c2):xs) 
addWtree ((B i0 a b):(L i3 c3):xs)= sortBy (increasing') 
            (B (i0+i3) (B i0 a b) (L i3 c3):xs)
addWtree ((L i3 c3):(B i0 a b):xs)= sortBy (increasing') 
            (B (i0+i3) (L i3 c3) (B i0 a b):xs)
addWtree ((B i0 a b):(B i3 a2 b2):xs)= sortBy (increasing') 
            (B (i0+i3) (B i0 a b) (B i3 a2 b2):xs)



encode :: String -> Htree -> [Integer]
encode [] _= (reverse [])
encode (x:xs) htree =(traverseDF') htree x [] ++ encode xs htree

traverseDF' :: Htree -> Char-> [Integer] -> [Integer]
traverseDF' (Leaf c1) c output  = if c1==c then output else []
traverseDF' (Branch l r) c output = (traverseDF' l c (1:output)) 
                ++ (traverseDF' r c (0:output))
{-
let x = statistics "huffman"
let y = makeWtree x
let z = sortWtree y
let w = createWtree z
createHtree w

2:
let x = statistics "text"
let y =maketree x
encode "text" y
traverseDF' y
-}