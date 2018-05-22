module Main where

main :: IO ()
main = do
  putStrLn "hello world"

data Expression
  = Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop
  = Plus     
  | Minus    
  | Times    
  | Divide   
  | Gt
  | Ge       
  | Lt  
  | Le
  | Eql
  deriving (Show, Eq)

data Statement
  = Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement       
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement        
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Part 01 -----------------------------------------

extend :: State -> String -> Int -> State
extend state name val = newState
  where newState s = if s == name then val else state s

empty :: State
empty s = 0

-- Part 02 -----------------------------------------

boolToInt :: Bool -> Int
boolToInt True  = 1
boolToInt False = 0

execOp :: Int -> Int -> Bop -> Int
execOp lval rval op = case op of
  Plus   -> lval + rval
  Minus  -> lval - rval
  Times  -> lval * rval
  Divide -> lval `div` rval
  Gt     -> boolToInt $ lval > rval
  Ge     -> boolToInt $ lval >= rval
  Lt     -> boolToInt $ lval < rval
  Le     -> boolToInt $ lval <= rval
  Eql    -> boolToInt $ lval == rval

evalE :: State -> Expression -> Int
evalE state (Var s) = state s
evalE state (Val v) = v
evalE state (Op exp1 op exp2) = execOp lval rval op
  where
    lval = evalE state exp1
    rval = evalE state exp2
  

-- Part 03 -----------------------------------------

data DietStatement
  = DAssign String Expression
  | DIf Expression DietStatement DietStatement
  | DWhile Expression DietStatement
  | DSequence DietStatement DietStatement
  | DSkip
  deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (Assign s e)        = DAssign s e
desugar (Incr s)            = DAssign s (Op (Var s) Plus (Val 1)) -- s <- s + 1
desugar (If e st1 st2)      = DIf e (desugar st1) (desugar st2)
desugar (While e st)        = DWhile e $ desugar st
desugar (For st1 e st2 st3) = DSequence (desugar st1) (DWhile e (DSequence (desugar st3) (desugar st2)))
desugar (Sequence st1 st2)  = DSequence (desugar st1) (desugar st2)
desugar Skip                = DSkip


-- Part 04 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple state (DAssign s e)       = extend state s $ evalE state e
evalSimple state (DIf e st1 st2)     = if evalE state e == 1 then evalSimple state st1 else evalSimple state st2
evalSimple state dst@(DWhile e st)   = if evalE state e == 1 then evalSimple (evalSimple state st) dst else state -- eval while again if e is true, but with new state
evalSimple state (DSequence st1 st2) = evalSimple (evalSimple state st1) st2 -- first evaluate st1, then st2 with new state
evalSimple state DSkip               = state -- just return current state

run :: State -> Statement -> State
run state = evalSimple state . desugar

-- Part 05 -----------------------------------------

parse :: String -> Maybe Statement
parse = undefined

-- Programs ----------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input
  for (Out := 1; In > 0; In := In - 1) {
      Out := In * Out
  }
-}

factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{-
  Calculate the floor of the square root of the input
  B := 0;
  while (A >= B * B) {
      B++
  };
  B := B - 1
-}

squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

{-
  Calculate the nth Fibonacci number

  F0 := 1;
  F1 := 1;

  if (In == 0) {
      Out := F0
  } else {
      if (In == 1) {
          Out := F1
      } else {
          for (C := 2; C <= In; C++) {
              T  := F0 + F1;
              F0 := F1;
              F1 := T;
              Out := T
          }
      }
  }

-}

fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]
