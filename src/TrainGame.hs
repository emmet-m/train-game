{-# LANGUAGE GADTs #-}

module TrainGame 
  ( solve,
    findSolutions,
    buildRpn,
    solutions,
    RPN (Op, Num),
    OperatorType (Pl, Mi, Mu, Di)
  ) where

import Data.List (permutations)

data OperatorType = Pl | Mi | Mu | Di deriving (Eq, Show)
data RPN = Op OperatorType | Num Integer deriving Show

operators :: [RPN] 
operators = [Op Pl, Op Mi, Op Mu, Op Di]

mapOperator :: OperatorType -> (Integer -> Integer -> Integer)
mapOperator Pl = (+)
mapOperator Mi = (-)
mapOperator Mu = (*)
mapOperator Di = quot

solutions :: [Integer] -> [[RPN]]
solutions xs = concat [ findSolutions $ buildRpn nums | nums <- permutations xs]

buildRpn :: [Integer] -> [[RPN]]
buildRpn xs = [ (Num first):(Num second):rpn | rpn <- buildRpn' (drop 2 xs) 2]
  where
    first  = xs !! 0
    second = xs !! 1
    buildRpn' :: [Integer] -> Integer -> [[RPN]]
    -- Nothing left
    buildRpn' [] 1 = [[]]
    -- Only operators left to insert
    buildRpn' [] c = [ o:rpn | o <- operators, rpn <- (buildRpn' [] (c-1)) ]
    buildRpn' (n:ns) c = 
      if c == 1 then
        -- Insert the current number in front of all possible permutations
        [ (Num n):rpn | rpn <- buildRpn' ns (c+1)]
      else 
        -- Insert the current number AND all possible operators and continue
        [ (Num n):rpn | rpn <- buildRpn' ns (c+1)]
        ++
        [ o:rpn | o <- operators, rpn <- (buildRpn' (n:ns) (c-1)) ]

findSolutions :: [[RPN]] -> [[RPN]]
findSolutions rpns = 
  filter  (\rpn -> 
            case solve rpn of
              Nothing -> False
              Just x  -> x == 10
          ) rpns

solve :: [RPN] -> Maybe Integer
solve rpn = solve' rpn []
  where
    solve' :: [RPN] -> [Integer] ->  Maybe Integer
    solve' ((Num n):xs) nums = solve' xs (n:nums) 
    solve' ((Op o):[]) (a:b:nums) = safeOp o a b Just
    solve' ((Op o):xs) (a:b:nums) = safeOp o a b (\n -> solve' xs (n:nums))

    safeOp :: OperatorType -> Integer -> Integer -> (Integer -> Maybe Integer) -> Maybe Integer
    safeOp o a b f = 
      if (o == Di && a == 0)
        then Nothing
        else f $ (mapOperator o) b a
      