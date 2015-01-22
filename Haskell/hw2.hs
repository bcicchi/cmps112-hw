{- 
	Program: hw2.hs
	Author: Brendan Cicchi - bcicchi - 1361334
	HW2 done solo
	sayNum (Problem 11) is done in Haskell and included in this File
-}

-- problem 1 (10)
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base [] = base
myFoldl f base (x:xs) = myFoldl f (f base x) xs

{-
	fold (+) 0 [0..5]
		= fold (+) ((+) 0 0 ) [1..5]
		= fold (+) ((+) 0 1) [2..5]
		= fold (+) ((+) 0+1 2) [3..5]
		= fold (+) ((+) 0+1+2 3) [4..5]
		= fold (+) ((+) 0+1+2+3 4) [5]
		= fold (+) ((+) 0+1+2+3+4 5) []
		= 1+2+3+4+5
-}

-- problem 2 (10)
myReverse :: [a] -> [a]
myReverse = foldl (\acc x -> x : acc) []
--myReverse = foldl (flip (:)) []

-- problem 3 (10)
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f base = foldl (flip f) base

-- problem 4 (10)
myFoldl2 :: (a -> b -> a) -> a -> [b] -> a
myFoldl2 f base = foldr (flip f) base

-- problem 5 (5)
isUpper :: Char -> Bool
isUpper = flip elem ['A'..'Z']

-- problem 6 (5) filter
onlyCapitals :: String -> String
onlyCapitals = filter isUpper

-- problem 7 (5) list comprehension
-- Used removeNonUppercase as reference from book chapter 1
onlyCapitals2 :: String -> String
onlyCapitals2 str = [ c | c <- str, isUpper c]

-- problem 8 (5) recursion
onlyCapitals3 :: String -> String
onlyCapitals3 [] = []
onlyCapitals3 (x:xs)
	| isUpper x = x : (onlyCapitals3 xs)
	| otherwise = onlyCapitals3 xs
	
-- problem 9 (5)
-- Assuming standard error for divide by 0 is ok
divRemainder :: Int -> Int -> (Int, Int)
divRemainder x y = (x `div` y, x `mod` y)

-- problem 10 (15)
digitSum :: Int -> Int
digitSum 0 = 0
digitSum x = (x `mod` 10) + digitSum (x `div` 10)

-- problem 11 (20)
{-
	I approached this problem in haskell with an 8 step process.
		1) trim any leading zeroes from the string
		2) reverse the string:
			"1234" -> "4321"
		3) separate reversed string into groups of 3 starting at beginning:
			"4321" -> ["432", "1"]
		4) reverse each group of 3:
			["432","1"] -> ["234", "1"]
		5) map convert function on each group of 3 to give word representation
			["234", "1"] -> ["two hundred thirty four", "one"]
		6) use modified zipWith' on big and the result of map:
			["two hundred thirty four", "one"] -> 
				["two hundred thirty four", "one thousand"]
		7) reverse the list:
			["two hundred thirty four", "one thousand"] -> 
				["one thousand", "two hundred thirty four"]
		8) unwords the [String]:
			["one thousand", "two hundred thirty four"] -> 
				"one thousand two hundred thirty four"
-}

sayNum :: String -> String
sayNum (x:xs) 
	| length t <= 69 = unwords$ reverse$ addBig (map convert (group t))
	| otherwise = "Error: Number is larger than 10^63 - 1"
	where
		t = trim (x:xs)
		big = ["", " thousand", " million", " billion", " trillion"] ++
			[" quadrillion", " quintillion", " sextillion", " septillion"]++
			[" octillion", " nonillion", " decillion", " undecillion"]++
			[" duodecillion", " tredecillion", " quattuordecillion"]++
			[" quindecillion", " sexdecillion", " septendecillion"]++
			[" septendecillion", " octodecillion", " novemdecillion"]++
			[" vigintillion"]
		addBig a = zipWith' (++) a big
		group [] = []
		group b =[reverse$take 3(reverse b)]++group(reverse$ drop 3(reverse b))
		convert [] = []
		convert (x:xs)
			| z==0 = []
			| z<10 =  tens !! (z - 1)
			| z<20 = (teens !! (z `mod` 10))
			| z<100 = (byTen!!((z `div` 10)-2))++" "++convert(show(z `mod` 10))
			| z<1000 = convert [x] ++ " hundred " ++ convert (take 2 xs)
			| otherwise = "Error: Too large a number"
			where
				tens = ["one", "two", "three", "four", "five", "six"] ++
					["seven", "eight", "nine"]
				teens = ["ten", "eleven", "twelve", "thirteen", "fourteen"] ++
					["fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]
				byTen = ["twenty", "thirty", "forty", "fifty", "sixty"] ++
					["seventy", "eighty", "ninety"]
				z = read (x:xs)

-- Created a modified zipWith so that only necessary "big" elements display
-- for sayNum since "1000000" -> "one million thousand" with regular zipWith		
zipWith' :: (String -> String -> String) -> [String] -> [String] -> [String]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = if null x
						then zipWith' f xs ys
						else f x y : zipWith' f xs ys

-- Remove leading zeroes if necessary => "0056" -> "56"
trim :: String -> String
trim [] = []
trim (x:xs)
	| x == '0' = trim xs
	| otherwise = (x:xs)