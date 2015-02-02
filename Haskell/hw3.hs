{- 
	Program: hw3.hs
	Author: Brendan Cicchi - bcicchi - 1361334
	HW3 done solo
-}
import Data.List
import Data.Char

data BST k v = Empty |
			Node k v (BST k v) (BST k v)
			
-- problem 1
val :: BST k v -> Maybe v
val Empty = Nothing
val (Node k1 v1 _ _) = Just v1

-- problem 2
size :: BST k v -> Int
size Empty = 0
size (Node k1 v1 l r) = 1 + size l + size r

--size = foldTree (\x -> x + 1) (const 0)

--BSTFold :: (b -> b -> b) -> (v -> b) -> (BST k v) -> b
--BSTFold op base Empty = Nothing
--BSTFold op base (Node k v l r) = 
--	(base v) `op` ((BSTFold op base l) `op` (BSTFold op base r))

-- problem 3
ins :: (Ord k) => k -> v -> BST k v -> BST k v
ins k v Empty = Node k v Empty Empty
ins k v (Node kp vp l r)
	| k < kp = Node kp vp (ins k v l) r
	| otherwise = Node kp vp l (ins k v r)
	
-- problem 4
instance (Show v) => Show (BST k v) where
	show Empty = ""
	show (Node k v l r) = "(" ++ show l ++ show v ++ show r ++ ")"
	
-- problem 5
data JSON = JStr String
	| JNum Double
	| JArr [JSON]
	| JObj [(String, JSON)]
	
instance Show JSON where
	show (JStr str) = show str --"\"" ++ str ++ "\""
	show (JNum n) = show n
	show (JArr arr) = "[" ++ (intercalate "," (map show arr)) ++ "]"
	show (JObj jobj) = "{" ++ (intercalate "," (map tupStr jobj)) ++ "}"

tupStr :: (String, JSON) -> String
tupStr (str, json) = show str ++ ":" ++ show json

-- problem 6
class Json a where
	toJson :: a -> JSON
	fromJson :: JSON -> a
	
instance Json Double where
	toJson = JNum
	fromJson (JNum jn) = jn
	
instance (Json a) => Json [a] where
	toJson xs = JArr $ map toJson xs
	fromJson (JArr xs) = map fromJson xs