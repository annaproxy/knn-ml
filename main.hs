import Data.Text hiding (take, map, zipWith, foldr, minimum, zip, length)
import Data.List 

main = do 
    k     <- getLine
    train <- readFile "train.txt"
    test  <- readFile "test.txt"
    return ( classify (read k :: Int) (makePeople train) (makePeople test) )
    -- return (makePeople x)

classify :: Int -> [Person] -> [Person] -> Double
classify k train test = fromIntegral(classifyAll k train test ) -- / fromIntegral(length test)

{-- 
    K -> TrainData -> TestData -> Amount Correct
--}
classifyAll :: Int -> [Person] -> [Person] -> Int 
classifyAll _ _ []         = 0
classifyAll k train (x:xs) = correct + classifyAll k train xs
    where
        correct = if (nearest x train k == y x) then 1 else 0 


nearest :: Person -> [Person] -> Int -> Int
nearest p1 pList k = if (k==1) then m else vote (map snd sorted) 0 
    where
        distances        = map (\x -> computeDistance p1 x) pList
        labels           = map y pList
        people           = zip distances labels -- ugly sort, especially for low number of neighbors.
        sorted           = take k ( sortBy ( \(a,_) (b,_) -> compare a b ) people )
        m                = snd $ minimumBy ( \(a,_) (b,_) -> compare a b  ) people
        -- Compensating for ugly sort with nice "lazy" vote function
        vote []     _    = 0
        vote (x:xs) amt1 = if ( check <= fromIntegral amt1) then 1 else vote xs (amt1 + x) 
            where
                check = (fromIntegral k) / (fromIntegral 2)

{-- Computes naive distance between two people --}
computeDistance :: Person -> Person -> Double
computeDistance p1 p2 =  sqrt ( d1 + d2 + d3 )
    where
        d1 = countStuff xReal
        d2 = fromIntegral $ countStuff xBinary
        d3 = fromIntegral $ countStuff xCategorical
        countStuff which = foldr (+) 0 (zipWith (\x y -> (x - y) * (x - y )) (which p1) (which p2) )

--computeDistance2 :: Person -> Person -> Double

{-- Breaks textfile into lines and creates People --}
makePeople :: String -> [Person]
makePeople x = map makePerson (split (=='\n') (pack x))

{-- Parses a line from the textfile and transforms it in a textfile 
    Yes, the attributes are hardcoded.
--}
makePerson :: Text -> Person
makePerson x = pResult
    where 
        lijst       = map unpack (split (==' ') x)
        categorical = map (\i -> read (lijst !! i) :: Int ) [3,4,5,11]
        continuous  = map (\i -> read (lijst !! i) :: Double ) [1,2,6,9,12,13]
        binary      = map (\i -> read (lijst !! i) :: Int ) [0,7,8,10]
        y           = read (lijst !! 14) :: Int
        pResult     = Person { xReal = continuous, xCategorical = categorical, xBinary = binary, y = y }

 
{--
    There are 6 numerical and 8 categorical attributes, and 1 outcome (y).
    They are stored in a person. Using ints makes us able to use the naive distance function
--}
data Person = Person {
    xReal        :: [ Double ],
    xBinary      :: [ Int ],
    xCategorical :: [ Int ],
    y            :: Int
} deriving (Show)