{-# LANGUAGE GADTs #-}

module TrainGame 
  ( solve,
    RPN (Op, Num),
    OperatorType (Pl, Mi, Mu, Di)
  ) where

import Data.List (permutations)

data OperatorType = Pl | Mi | Mu | Di deriving Eq
data RPN = Op OperatorType | Num Integer

operators :: [RPN] 
operators = [Op Pl, Op Mi, Op Mu, Op Di]

mapOperator :: OperatorType -> (Integer -> Integer -> Integer)
mapOperator Pl = (+)
mapOperator Mi = (-)
mapOperator Mu = (*)
mapOperator Di = quot

solutions :: [Integer] -> [String]
solutions xs = []

buildRpn :: [Integer] -> [[RPN]]
buildRpn xs = map (\rpn -> (Num first):(Num second):rpn) 
                           (buildRpn' (drop 2 xs) 2)
  where
    first  = xs !! 0
    second = xs !! 1
    buildRpn' :: [Integer] -> Integer -> [[RPN]]
    -- Nothing left
    buildRpn' [] 1 = [[]]
    -- Only operators left to insert
    buildRpn' [] c = concat $
                       map (\rpn -> map (\o -> o:rpn) operators) $ buildRpn' [] (c-1)
    buildRpn' (n:ns) c = 
      if c == 1 then
        -- Insert the current number in front of all possible permutations
        map (\rpn -> ((Num n):rpn)) (buildRpn' ns (c+1))
      else 
        -- Insert the current number AND all possible operators and continue
        map (\rpn -> ((Num n):rpn)) (buildRpn' ns (c+1)) 
        ++
        concat (map (\rpn -> map (\op -> op:rpn) operators) (buildRpn' ns (c-1)))

findSolutions :: [[RPN]] -> [[RPN]]
findSolutions rpns = filter (\rpn -> case solve rpn of
                                    Nothing -> False
                                    Just x  -> x == 10
                            ) rpns

solve :: [RPN] -> Maybe Integer
solve rpn = solve' rpn []
  where
    solve' :: [RPN] -> [Integer] ->  Maybe Integer
    solve' ((Num n):xs) nums = solve' xs (n:nums) 
    solve' ((Op o):[]) (a:b:nums) = 
      if (o == Di && b == 0)
        then Nothing
        else Just $ (mapOperator o) b a
    solve' ((Op o):xs) (a:b:nums) = solve' xs (((mapOperator o) b a):nums)

{-

data ASTNode where
  Const     :: Integer -> ASTNode
  Plus      :: ASTNode -> ASTNode -> ASTNode
  Minus     :: ASTNode -> ASTNode -> ASTNode
  Multiply  :: ASTNode -> ASTNode -> ASTNode
  Divide    :: ASTNode -> ASTNode -> ASTNode

solve :: ASTNode -> Maybe Integer
solve (Const i)  = Just i
solve (Plus l r) = do
  li <- solve l
  lr <- solve r
  return (li + lr)
solve (Minus l r) = do
  li <- solve l
  lr <- solve r
  return (li - lr)
solve (Multiply l r) = do
  li <- solve l
  lr <- solve r
  return (li * lr)
solve (Divide l r) = do
  li <- solve l
  lr <- solve r
  if lr == 0 then 
    Nothing 
  else 
    return $ quot li lr
-}