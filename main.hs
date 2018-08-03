import Data.Text hiding (take, map, zipWith, foldr)

main = do 
    x <- readFile "train.txt"
    print( take 5 (makePeople x))
    -- return (makePeople x)


calculateClosest :: Person -> [Person] -> Int
calculateClosest p1 p2 = 0 -- "to do, i have to sleep"

{-- Computes naive distance between two people --}
computeDistance :: Person -> Person -> Double
computeDistance p1 p2 =  sqrt ( d1 + d2 + d3 )
    where
        d1 = countStuff xReal
        d2 = fromIntegral $ countStuff xBinary
        d3 = fromIntegral $ countStuff xCategorical
        countStuff which = foldr (+) 0 (zipWith (\x y -> (x - y) * (x - y )) (which p1) (which p2) )

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