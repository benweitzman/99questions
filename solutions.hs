import System.Random

-- Problem 1
myLast :: [a] -> a
myLast [] = error "empty list"
myLast [c] = c
myLast (c:cs) = myLast cs

-- Problem 2
butLast :: [a] -> a
butLast [] = error "empty list"
butLast [a] = error "singleton list"
butLast [a, b] = a
butLast (a:as) = butLast as

-- Problem 3
elementAt :: [a] -> Int -> a
elementAt [] _ = error "Index out of bounds"
elementAt (x:xs) 1 = x
elementAt (x:xs) n 
  | n > 0 = elementAt xs (n - 1)
  | otherwise = error "Index out of bounds"
                
-- Problem 4                
myLength :: [a] -> Int                
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

-- Problem 5
myReverse :: [a] -> [a]
myReverse xs = revApp xs []
  where revApp [] ys = ys
        revApp (x:xs) ys = revApp xs $ x:ys

-- Problem 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = listsEqual xs ys
  where ys = myReverse xs
        listsEqual [] [] = True
        listsEqual (x:xs) (y:ys) = x == y && listsEqual xs ys
        listsEqual _ _ = False
        
-- Problem 7        
data NestedList a = Elem a | List [NestedList a]        

flatten :: NestedList a -> [a]
flatten (List []) = []
flatten (Elem x) = [x]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

-- Problem 8
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:xs) = x:(compress (remove x xs))
 where remove _ [] = []
       remove x (y:ys) = if x == y
                         then remove x ys
                         else y:ys
                              
-- Problem 9                              
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack xs = packAcc (myReverse xs) []
  where packAcc [] acc = acc
        packAcc (x:xs) [] = packAcc xs [[x]]
        packAcc (x:xs) ((y:ys):zs) = if x == y
                                     then packAcc xs ((x:y:ys):zs)
                                     else packAcc xs ([x]:(y:ys):zs)
                                          
-- Problem 10                                          
encode :: (Eq a) => [a] -> [(Int, a)]
encode = map (\x -> (myLength x, head x)) . pack

-- Problem 11
data Encoding a = Single a | Multiple Int a deriving (Show)

encodeModified :: (Eq a) => [a] -> [Encoding a]
encodeModified = map (\x -> case x of
                         (1, y) -> Single y
                         (n, y) -> Multiple n y)
                     . encode
                 
-- Problem 12
decodeModified :: (Eq a) => [Encoding a] -> [a]
decodeModified [] = []
decodeModified (x:xs) = case x of
                             Single v -> v:(decodeModified xs)
                             Multiple n v -> (replicate n v) ++ (decodeModified xs)
                             
-- Problem 13                             
encodeDirect :: (Eq a) => [a] -> [Encoding a]
encodeDirect xs = add (reverse xs) []
  where add [] acc = acc
        add (x:xs) [] = add xs [Single x]
        add (x:xs) ((Single y):ys) 
          | x == y = add xs (Multiple 2 y:ys)
          | otherwise = add xs (Single x:Single y:ys)
        add (x:xs) ((Multiple n y):ys) 
          | x == y = add xs (Multiple (n + 1) y:ys)
          | otherwise = add xs (Single x:Multiple n y:ys)
                        
-- Problem 14                        
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:dupli xs

-- Problem 15
repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n = (replicate n x) ++ (repli xs n)

-- Problem 16
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = iter (reverse xs) (length xs `mod` n) []
  where iter [] _ acc = acc
        iter (y:ys) q acc
          | q == 0 = iter ys (n - 1) acc
          | otherwise = iter ys (q - 1) (y:acc)
                        
-- Problem 17                        
split :: [a] -> Int -> ([a], [a])
split xs n = (take n xs, drop n xs)

-- Problem 18
slice :: [a] -> Int -> Int -> [a]
slice xs i k = drop (i - 1) $ take k xs

-- Problem 19
rotate :: [a] -> Int -> [a]
rotate xs n 
  | n >= 0 = drop n xs ++ take n xs
  | otherwise = drop (length xs + n) xs ++ take (length xs + n) xs
                
-- Problem 20                
removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = (elementAt xs n, take (n - 1) xs ++ drop n xs)

-- Problem 21
insertAt :: a -> [a] -> Int -> [a]
insertAt v [] _ = [v]
insertAt v xs n = take (n - 1) xs ++ [v] ++ drop (n - 1) xs

-- Problem 22
range :: Int -> Int -> [Int]
range x y = [x..y]

-- Problem 23
rnd_select :: [a] -> Int -> IO [a]
rnd_select xs n 
  | n > length xs = error "More elements than exist in list"
rnd_select xs 0 = return []                    
rnd_select xs n = do g <- newStdGen                    
                     (r, g') <- randomR (0, length xs - 1) g
                     (xs !! r):(rnd_select (removeAt r xs) (n - 1))                    
                   