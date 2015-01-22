{- 
	Program: hw1.hs
	Author: Brendan Cicchi - bcicchi - 1361334
	HW1 done solo
-}
-- problem 1 (5) ghci installed, not sure how you check that
--			I'm guessing just by hw submission itself is enough proof

-- problem 2 (5)
citeAuthor :: String -> String -> String
citeAuthor first last = last ++ ", " ++ first

-- problem 3 (5)
initials :: String -> String -> String
initials (f:_) (l:_) = [f, '.', l, '.']
{-
	Other options for initials:
		initials first last = take 1 first ++ "." ++ take 1 last ++ "."
		initials first last = [head first, '.', head last, '.']
-}

--problem 4 (5)
title :: (String, String, Int) -> String
title (_, title, _) = title

--problem 5 (10) \n will show unless called with putStr
citeBook :: (String, String, Int) -> String
citeBook (a, t, y) = t  ++ " (" ++ a ++ ", " ++ show y ++ ")"

--problem 6 (10) similar to maximum function in recursion chapter 
	-- needs to be called in ghci using putStr (bibliography_rec x)
bibliography_rec :: [(String, String, Int)] -> String
bibliography_rec [] = []
bibliography_rec (x:xs) = citeBook x ++ "\n" ++ bibliography_rec xs

--problem 7 (10) 
bibliography_fold :: [(String, String, Int)] -> String
bibliography_fold x = foldl (\acc x -> acc ++ (citeBook x) ++ "\n") [] x
{-
	Options which I could not get the \n to work with
		bibliography_fold = foldr ((++) . citeBook) []
		bibliography_fold x = foldl (++) [] (map citeBook x))
-}

--problem 8 (15)
averageYear :: [(String, String, Int)] -> Int
averageYear x = (sum $ map getYear x) `div` length x
	where getYear (_, _, year) = year

--problem 9 (15)
references :: String -> Int
references x = length $ filter isRef $ words x
	where isRef x = if length x /= 0 && head x == '[' && last x == ']'
						then True
						else False


--problem 10 (20)
citeText :: [(String, String, Int)] -> String -> String
citeText cites txt = unwords (map replace (words txt))
	where replace x = if length x/= 0 && head x == '[' && last x == ']'
					then citeBook $ (!!) cites (read [( x !! 1)] - 1)
					else x
