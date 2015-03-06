-- Necessary imports
import Control.Applicative (Applicative(..),(<$>),liftA,liftA2,(<*>))
import Control.Monad       (liftM,ap)
--import Control.Applicative
import Data.Map
import Data.Maybe
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as P

--------- AST Nodes ---------

-- Variables are identified by their name as string
type Variable = String

-- Values are either integers or booleans
data Value = IntVal Int       -- Integer value
           | BoolVal Bool     -- Boolean value

-- Expressions are variables, literal values, unary and binary operations
data Expression = Var Variable                    -- e.g. x
                | Val Value                       -- e.g. 2
                | BinOp Op Expression Expression  -- e.g. x + 3
                | Assignment Variable Expression  -- e.g. x = 3

-- Statements are expressions, conditionals, while loops and sequences
data Statement = Expr Expression                   -- e.g. x = 23
               | If Expression Statement Statement -- if e then s1 else s2 end
               | While Expression Statement        -- while e do s end
               | Sequence Statement Statement      -- s1; s2
               | Skip                              -- no-op
               | For Variable Expression Expression Statement -- for var in e1 to e2 do s end

-- All binary operations
data Op = Plus         --  +  :: Int -> Int -> Int
        | Minus        --  -  :: Int -> Int -> Int
        | Times        --  *  :: Int -> Int -> Int
        | GreaterThan  --  >  :: Int -> Int -> Bool
        | Equals       --  == :: Int -> Int -> Bool
        | LessThan     --  <  :: Int -> Int -> Bool

-- The `Store` is an associative map from `Variable` to `Value` representing the memory
type Store = Map Variable Value

--------- Parser ---------

-- The Lexer

lexer = P.makeTokenParser (emptyDef {
  P.identStart = letter,
  P.identLetter = alphaNum,
  P.reservedOpNames = ["+", "-", "*", "!", ">", "=", "==", "<"],
  P.reservedNames = ["true", "false", "if", "in", "then", "else", "while", "end", "to", "do", "for"]
})

-- The Parser

-- Number literals
numberParser :: Parser Value
numberParser = (IntVal . fromIntegral) <$> P.natural lexer

-- Boolean literals
boolParser :: Parser Value
boolParser =  (P.reserved lexer "true" >> return (BoolVal True))
          <|> (P.reserved lexer "false" >> return (BoolVal False))

-- Literals and Variables
valueParser :: Parser Expression
valueParser =  Val <$> (numberParser <|> boolParser)
           <|> Var <$> P.identifier lexer

-- -- Expressions
exprParser :: Parser Expression
exprParser = liftA2 Assignment
                    (try (P.identifier lexer >>= (\v ->
                          P.reservedOp lexer "=" >> return v)))
                    exprParser
          <|> buildExpressionParser table valueParser
    where table = [[Infix (op "*" (BinOp Times)) AssocLeft]
                  ,[Infix (op "+" (BinOp Plus)) AssocLeft]
                  ,[Infix (op "-" (BinOp Minus)) AssocLeft]
                  ,[Infix (op ">" (BinOp GreaterThan)) AssocLeft]
                  ,[Infix (op "==" (BinOp Equals)) AssocLeft]
                  ,[Infix (op "<" (BinOp LessThan)) AssocLeft]]
          op name node = (P.reservedOp lexer name) >> return node

-- Sequence of statements
stmtParser :: Parser Statement
stmtParser = stmtParser1 `chainl1` (P.semi lexer >> return Sequence)

-- Single statements
stmtParser1 :: Parser Statement
stmtParser1 = (Expr <$> exprParser)
          <|> do
              P.reserved lexer "if"
              cond <- exprParser
              P.reserved lexer "then"
              the <- stmtParser
              P.reserved lexer "else"
              els <- stmtParser
              P.reserved lexer "end"
              return (If cond the els)
          <|> do
              P.reserved lexer "while"
              cond <- exprParser
              P.reserved lexer "do"
              body <- stmtParser
              P.reserved lexer "end"
              return (While cond body)
          <|> do
              P.reserved lexer "for"
              var <- P.identifier lexer
              P.reserved lexer "in"
              low <- exprParser
              P.reserved lexer "to"
              up <- exprParser
              P.reserved lexer "do"
              body <- stmtParser
              P.reserved lexer "end"
              return (For var low up body)

-------- Helper functions --------

-- Lift primitive operations on IntVal and BoolVal values
liftIII :: (Int -> Int -> Int) -> Value -> Value -> Value
liftIII f (IntVal x) (IntVal y) = IntVal $ f x y
liftIIB :: (Int -> Int -> Bool) -> Value -> Value -> Value
liftIIB f (IntVal x) (IntVal y) = BoolVal $ f x y

-- Apply the correct primitive operator for the given Op value
applyOp :: Op -> Value -> Value -> Value
applyOp Plus        = liftIII (+)
applyOp Minus       = liftIII (-)
applyOp Times       = liftIII (*)
applyOp GreaterThan = liftIIB (>)
applyOp Equals      = liftIIB (==)
applyOp LessThan    = liftIIB (<)

-- Parse and print (pp) the given WHILE programs
pp :: String -> IO ()
pp input = case (parse stmtParser "" input) of
    Left err -> print err
    Right x  -> print x

-- Parse and run the given WHILE programs
run :: (Show v) => (Parser n) -> String -> (n -> Store -> v) -> IO ()
run parser input eval = case (parse parser "" input) of
    Left err -> print err
    Right x  -> print (eval x empty)

-- Parse and run the given WHILE programs using monads
runMonad :: String -> Maybe Store
runMonad input = proc (parse stmtParser "" input)
    where proc (Right x) = snd `fmap` runImperative (evalS_monad x) empty
          proc _         = Nothing

---------------------------------- problem 1 ----------------------------------
-- instances of show
instance Show Value where
	show (IntVal x)  = show x
	show (BoolVal x) = if (x) then "true" else "false"
	
instance Show Op where
	show Plus = " + "
	show Minus = " - "
	show Times = " * "
	show GreaterThan = " > "
	show Equals = " == "
	show LessThan = " < "
	
instance Show Expression where
	show (Var var) = var
	show (Val val) = show val
	show (BinOp op exprl exprr) = show exprl ++ show op ++ show exprr
	show (Assignment var expr) = var ++ " = " ++ show expr

instance Show Statement where
  show (Expr expr) = show expr
  show (If e s1 s2) = "if " ++ show e ++ " then " ++ show s1 ++ " else " ++ show s2 ++ " end "
  show (While expr s) = "while " ++ show expr ++ " do " ++ show s ++ " end"
  show (For v e1 e2 s1) = "for " ++ v ++ " in " ++ show e1 ++ " to " ++ show e2 ++ " do " ++ show s1 ++ " end"
  show (Sequence s1 s2) = show s1 ++ ";" ++ show s2
  show Skip = ""

  
	
---------------------------------- problem 2 ----------------------------------
evalE :: Expression -> Store -> (Value, Store)
-- BinOp evalE --> given by instructor
evalE (BinOp o a b) s = (applyOp o a' b', s'')
	where 
		(a', s') = evalE a s
		(b', s'') = evalE b s'
-- Var evalE --> if variable is in map return (val, store) otherwise throw an error
evalE (Var x) s = (x', s)
    where x' = fromMaybe (error "Var not assigned") (Data.Map.lookup x s)
-- Val evalE --> return (val, store)
evalE (Val v) s = (v, s)
-- Assignment evalE --> insert variable into map and evaluate e
evalE (Assignment x e) s = (e', s'')
	where
		(e', s') = evalE e s
		s'' = Data.Map.insert x e' s'
		
---------------------------------- problem 3 ----------------------------------
evalS :: Statement -> Store -> Store
-- While evalS --> given by instructor
evalS w@(While e s1) s = case (evalE e s) of
                          (BoolVal True,s')  -> let s'' = evalS s1 s' in evalS w s''
                          (BoolVal False,s') -> s'
                          _                  -> error "Condition must be a BoolVal"
-- Skip evalS --> no-op so do nothing and return previous store
evalS Skip s = s
-- Expr evalS --> 
evalS (Expr e) s = s'
	where (_,s') = evalE e s
evalS (Sequence s1 s2) s = s''
	where 
		s' = evalS s1 s
		s'' = evalS s2 s'
-- If evalS --> if evaluates to true eval
evalS (If e s1 s2) s = case (evalE e s) of
						  (BoolVal True,s')  -> evalS s1 s'
						  (BoolVal False,s') -> evalS s2 s'
						  _					 -> error "Condition must be a BoolVal"
-- For evalS --> do assignment of v with e1 and use that store if for loop evaluates false
--               otherwise do the evaluation of the statement and return that store
evalS (For v e1 e2 s1) s = case (e1' <= e2') of
              True  -> evalS(For v (Val(IntVal (e1' + 1))) e2 s1 ) s''
              False -> s'
              where
                (e1', s') = case (evalE (Assignment v e1) s) of
                          ((IntVal x), s') -> (x, s')
                          _                -> error "For loop must contain an IntVal"
                e2' = case (evalE e2 s') of
                          ((IntVal w), s') -> w
                          _                -> error "For loop must contain an IntVal"
                s'' = evalS s1 s'
---------------------------------- problem 4 ----------------------------------
evalE_maybe :: Expression -> Store -> Maybe (Value, Store)					  
evalE_maybe (BinOp o a b) s = do (a',s') <- evalE_maybe a s
                                 (b',s'') <- evalE_maybe b s'
                                 return (applyOp o a' b', s'')
evalE_maybe (Var x) s = do x' <- Data.Map.lookup x s
                           return (x', s)
evalE_maybe (Val v) s = return (v, s)
evalE_maybe (Assignment x e) s = do (e', s') <- evalE_maybe e s
                                    let s'' = Data.Map.insert x e' s' 
										                  in return (e', s'')
									
evalS_maybe :: Statement -> Store -> Maybe Store
evalS_maybe w@(While e s1) s = case (evalE_maybe e s) of
                          Just (BoolVal True,s')  -> case (evalS_maybe s1 s') of
														  Just s'' -> evalS_maybe w s''
														  Nothing -> Nothing
                          Just (BoolVal False,s') -> return s'
                          _                       -> Nothing
evalS_maybe Skip s = return s
evalS_maybe (Sequence s1 s2) s = evalS_maybe s2 =<< evalS_maybe s1 s
evalS_maybe (Expr e) s = snd <$> evalE_maybe e s
evalS_maybe (If e s1 s2) s = case (evalE_maybe e s) of
            						  Just (BoolVal True, s')  -> evalS_maybe s1 s'
            						  Just (BoolVal False, s') -> evalS_maybe s2 s'
            						  _						   -> Nothing
evalS_maybe (For v e1 e2 s1) s = do (e1', st1) <- evalE_maybe (Assignment v e1) s
                                    (e2', st2) <- evalE_maybe e2 st1
                                    (i, st3)   <- evalE_maybe (BinOp Plus e1 (Val $ IntVal 1)) st2
                                    case (evalE_maybe (BinOp GreaterThan e1 e2) st3) of
                                            Just(BoolVal True, st4)  -> Just st4
                                            Just(BoolVal False, st4) -> case (evalS_maybe s1 st4) of
                                                                              Just st5 -> evalS_maybe (For v (Val i) e2 s1) st5
                                                                              Nothing -> Nothing
                                            _                        -> Nothing

---------------------------------- problem 5 ----------------------------------
newtype Imperative a = Imperative {
    runImperative :: Store -> Maybe (a, Store)
}

instance Functor Imperative where
    fmap = liftM

instance Applicative Imperative where
    pure  = return
    (<*>) = ap

instance Monad Imperative where
    return a = Imperative (\s -> Just (a,s))
    b >>= f = Imperative (\s -> do (v1,s1) <- (runImperative b) s
                                   runImperative (f v1) s1)
    fail _ = Imperative (\s -> Nothing)


getVar :: Variable -> Imperative Value
getVar var = Imperative (\store -> ((Data.Map.lookup var store) >>= (\v -> Just (v,store))))

setVar :: Variable -> Value -> Imperative Value
setVar var val = Imperative (\store -> Just (val, Data.Map.insert var val store))

	
evalE_monad :: Expression -> Imperative Value
evalE_monad (BinOp o a b) = do a' <- evalE_monad a
                               b' <- evalE_monad b
                               return (applyOp o a' b')
evalE_monad (Var x) = getVar x
evalE_monad (Val v) = return v
evalE_monad (Assignment x e) = do e' <- evalE_monad e ; setVar x e'

evalS_monad :: Statement -> Imperative ()
evalS_monad (While e s1)     = do e' <- evalE_monad e ; case e' of
									BoolVal True  -> do evalS_monad s1 ; evalS_monad (While e s1)
									BoolVal False -> return ()
									_			        -> fail "Condition must be a BoolVal"
evalS_monad Skip             = return ()
evalS_monad (Sequence s1 s2) = do evalS_monad s1 ; evalS_monad s2
evalS_monad (Expr e)         = do evalE_monad e ; return ()
evalS_monad (If e s1 s2)     = do e' <- evalE_monad e ; case e' of
									BoolVal True  -> evalS_monad s1
									BoolVal False -> evalS_monad s2
									_			        -> fail "Condition must be a BoolVal"
 
evalS_monad (For v e1 e2 s1) = do evalE_monad (Assignment v e1) 
                                  e' <- evalE_monad (BinOp GreaterThan e1 e2)
                                  case ( e') of
                                      BoolVal False  -> do evalS_monad s1
                                                           e0 <- evalE_monad (BinOp Plus e1 (Val(IntVal 1)))
                                                           evalS_monad (For v (Val e0) e2 s1)
                                      BoolVal True -> return ()
                                      _                -> fail "Conditions must be IntVals"
				             

miniprog :: Imperative Value
miniprog = do
            setVar "x" (IntVal 2)
            setVar "y" (IntVal 3)
            a <- getVar "x"
            b <- getVar "y"
            return (applyOp Plus a b)
			
			
---------------------------------- problem 6 ----------------------------------
-- the For was sprinkled into the code as necessary for the three places