import Text.XML.HXT.Core
import Text.HandsomeSoup
import Data.List.Utils
import Text.Regex.Posix
import Data.List
import System.IO.Unsafe
gatherLinks ::  String ->   IO [String] -- this function goes to the page and grabs all the importnt links
gatherLinks link = do
          let doc = fromUrl $ "http://en.wikipedia.org/wiki/" ++ link -- the url we are visting
          links <- runX $ doc >>> css "a" >>> getAttrValue "href" --find all the links
          let connections = nub $ [ (split "/wiki/" x)!!1 | x <- links, startswith "/wiki/" x, not $ isInfixOf ":" x,not $ isInfixOf "#" x] -- cleans up the links
          return (take 3 $ connections) -- return the list of connections, to use more than 3 change the number of links you grab

getFirst (x, _) = x -- return the first vaule in an ordered pair   
getSecond (_,x) = x -- return the second

displayPair:: (String, IO [String]) -> IO [String] --show the topic and print it with each of its connections at diffrent times.  
displayPair (a,b) = do
     tempB <- b
     return [a ++ " " ++ y | y<- tempB]

mapLinks :: [String] -> [(String, IO [String])] -- this function is returning a list of all the vaule a node is conncted it. 
mapLinks (x:[])  = [(x,  gatherLinks x)] -- if there is only one element in the list stop the recursion and start passing up
mapLinks (x:xs) =  (x,(gatherLinks x)):(mapLinks (nub xs)) -- if since there is mutiple valus calculate the first element

printMap (x:[]) = do --prints a topic recusively
    mapM_  putStrLn =<< ( displayPair $ x)
printMap (x:mapList) = do 
    mapM_  putStrLn =<< ( displayPair $ x)
    printMap mapList

printGraph (x:[]) = printMap x -- print all of the values
printGraph (x:mapList) = do
     printMap x
     printGraph mapList

printForR [] list2 = do --all this does is print the list in the corrct format for R to read
     return []
printForR (x:y:list1) [] = do
     putStrLn (x ++ " " ++ y)
     printForR list1 (y:[])
printForR (x:list1) (y:list2) = do
     putStrLn (y ++ " " ++ x)
     printForR list1 (x:list2)

gatherNodes::  [String] -> Integer -> [String]  -- go grab links until you hit the cap
gatherNodes topics 0 =  concat [unsafePerformIO $ gatherLinks a |  a <- (topics)] 
gatherNodes topics count =  concat ((concat [ unsafePerformIO $ gatherLinks a |  a <- (topics)]):(gatherNodes (concat [ unsafePerformIO $ gatherLinks a |  a <- (topics)]) (count-1)):[])

getNeighbors allLinks topic = nub $ concat [ [t:a:[] | a <- (unsafePerformIO  c)] | (t, c) <- (allLinks), topic == t ]

bSearch allLinks chains search 
     | not $ null $ concat [ x| x <- chains, (head $ reverse x) == search] = [ x| x <- chains, (head $ reverse x) == search] --chains
     | otherwise =  bSearch allLinks (expand allLinks chains) search

--expand takes a list of topics, then returns the connections of each topic
expand allLinks chains =  concat [[ ((reverse $ tail $ reverse chain) ++ y) | y<-(getNeighbors allLinks  (head  (reverse chain))), not (elem (head $ reverse y) chain) ] | chain <- chains]

main = do
    let mainTopic = ["Science"]--the topic we start with
    let targetTopic = "Lo_Stivale_(newspaper)"--the topic we start with
    let mappedLinks =  mapLinks $ gatherNodes mainTopic 4 --The 4 is how deep you go
    printGraph ((mappedLinks):[]) --print in correct format 
    putStrLn "\n"
    let neighbors = bSearch mappedLinks (mainTopic:[]) ( targetTopic) --expand mappedLinks $ expand mappedLinks $ expand mappedLinks $ expand mappedLinks $ expand mappedLinks (mainTopic:[])  --
    printForR (concat neighbors) []  --print the search results to the output in the correct format







