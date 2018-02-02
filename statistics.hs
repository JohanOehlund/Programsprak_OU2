import Data.List

data Htree = Leaf Char | Branch Htree Htree deriving(Show)
data Wtree = L Integer Char | B Integer Wtree Wtree deriving(Show)

{-
Function: statistics
Comment: Counts the number of instances for each character in a string.
-}
statistics :: String -> [(Integer, Char)] 
statistics [] = []
statistics (x:xs) = countLetters (x:xs) : statistics [res| res<-xs, res/=x] 

countLetters :: String -> (Integer,Char)
countLetters (x:xs) = (sum [1|c<-(x:xs),c==x],x)


makeWtree :: [(Integer, Char)] -> [Wtree]
makeWtree ((i,c):[]) = [L i c]
makeWtree ((i,c):xs) = L i c : makeWtree xs

sortWtree :: [Wtree] -> [Wtree]
sortWtree [] = []
sortWtree (x:xs) = sortBy (less') (x:xs)
--(sortBy) less' (x:xs)

less' :: Wtree -> Wtree-> Ordering
less' (L x _) (L y _) = compare x y


createWtree :: [Wtree] -> [Wtree]  
createWtree x
    | length x == 1 = x
    | otherwise = createWtree (addWtree x)


addWtree :: [Wtree] -> [Wtree]
addWtree ((L i1 c1):(L i2 c2):xs) = (B (i1+i2) (L i1 c1) (L i2 c2):xs) 
addWtree ((B i0 a b):(L i3 c3):xs)= (B (i0+i3) (B i0 a b) (L i3 c3):xs)
addWtree ((L i3 c3):(B i0 a b):xs)= (B (i0+i3) (L i3 c3) (B i0 a b):xs)
addWtree ((B i0 a b):(B i3 a2 b2):xs)= (B (i0+i3) (B i0 a b) (B i3 a2 b2):xs)

{-
convertAndQuicksort [] = []  
convertAndQuicksort ((reg,(hour,minute),(totHour,totMinute)):xs) =
    (convertAndQuicksort [(a,b,time)|(a,b,time)<-xs, time>(totHour,totMinute)]) 
                        ++[(reg,(totHour,totMinute))]++ 
    (convertAndQuicksort [(a,b,time)|(a,b,time)<-xs, time<=(totHour,totMinute)])
-}
{-
let x = statistics "hejsan"
let y = makeWtree x
let z = sortWtree y
createWtree z
-}