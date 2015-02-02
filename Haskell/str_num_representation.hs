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

-- main function to convert number to word representation
sayNum :: String -> String
sayNum x = unwords $ reverse $ addBig $ map convert $ group $ trim x

------------------------------ Helper Functions ------------------------------
-- group reversed numbers into groups of 3 where the last group <= 3
group :: String -> [String]
group [] = []
group x = z ++ group a
	where
		y = reverse x
		z = [reverse $ take 3 y]
		a = reverse $ drop 3 y

-- convert each group of <= 3 numbers to a word representation
convert :: String -> String
convert [] = []
convert (x:xs)
	| z == 0 = []
	| z < 10 =  tens !! (z - 1)
	| z < 20 = (teens !! (z `mod` 10)) 
	| z < 100 = (byTen !!((z `div` 10) - 2)) ++" "++ convert (show(z `mod` 10))
	| z < 1000 = convert [x] ++ " hundred " ++ convert (take 2 xs)
	| otherwise = "Error: Too large a number"
	where 
		z = read (x:xs)
		tens = ["one", "two", "three", "four", "five", "six", "seven"] ++
			["eight", "nine"]
		teens = ["ten", "eleven", "twelve", "thirteen", "fourteen"] ++ 
			["fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]
		byTen = ["twenty", "thirty", "forty", "fifty", "sixty", "seventy"] ++
			["eighty", "ninety"]

-- zip a list of strings with the "big" list
addBig :: [String] -> [String]
addBig x = zipWith' (++) x big
	where
		big = ["", " thousand", " million", " billion", " trillion"] ++
			[" quadrillion", " quintillion", " sextillion", " septillion"]++
			[" octillion", " nonillion", " decillion", " undecillion"]++
			[" duodecillion", " tredecillion", " quattuordecillion"]++
			[" quindecillion", " sexdecillion", " septendecillion"]++
			[" septendecillion", " octodecillion", " novemdecillion"]++
			[" vigintillion"]

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