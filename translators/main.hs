module Main where

import Grammar
import PrintAgda
import PrintIdris
import PrintLean
import PrintRocq

-- this is equivalent to:
-- n :: Int
-- n = let x1 = 1 in
--     let x2 = x1 in
--     let x3 = x2 in
--         x3
test :: Module
test = Module "LetExample"
    [
        DefVar "n" (Just $ Con "Nat") (Let [DefVar "x1" Nothing $ Int 1] $ Let [DefVar "x2" Nothing $ Var "x1"] $ Let [DefVar "x3" Nothing $ Var "x2"] $ Var "x3")
    ]

-- Test case for concatentating a list at the beginning of another list:
-- concatenateFront :: [Int] -> [Int] -> [Int]
-- concatenateFront list1 list2 = list1 ++ list2

listConcatTest1 :: Module
listConcatTest1 = Module "ConcatenateFront"
                [
                    DefFun "concatenateFront" (Just (Arr (List (Con "Nat")) (Arr (List (Con "Nat")) (List (Con "Nat"))))) 
                    [Arg "list1" (List (Con "Nat")), Arg "list2" (List (Con "Nat"))] 
                    (Bin "++" (Var "list1") (Var "list2"))
                ]

-- concatenateFront :: [Int] -> [Int] -> [Int]
-- concatenateFront list1 list2 = list2 ++ list1

listConcatTest2 :: Module
listConcatTest2 = Module "ConcatenateBack"
                [
                    DefFun "concatenateBack" (Just (Arr (List (Con "Nat")) (Arr (List (Con "Nat")) (List (Con "Nat"))))) 
                    [Arg {arg = "list1", ty = List (Con "Nat")}, Arg {arg = "list2", ty = List (Con "Nat")}]
                    (Bin "++" (Var "list2") (Var "list1"))
                ]  
               
-- prependToList :: Int -> [Int] -> [Int]
-- prependToList x xs = x : xs

prependToList :: Module
prependToList = Module "PrependToList"
              [
                DefFun "prependToList" (Just (Arr (Con "Nat") (Arr (List (Con "Nat")) (List (Con "Nat")))))
                [Arg {arg = "x", ty = Con "Nat"}, Arg {arg = "xs", ty = List (Con "Nat")}]
                (Bin ":" (Var "x") (Var "xs"))
              ]

-- appendToList :: Int -> [Int] -> [Int]
-- appendToList x xs = xs ++ [x]

appendToList :: Module
appendToList = Module "AppendToList"
              [
                DefFun "appendToList" (Just (Arr (Con "Nat") (Arr (List (Con "Nat")) (List (Con "Nat")))))
                [Arg {arg = "x", ty = Con "Nat"}, Arg {arg = "xs", ty = List (Con "Nat")}]
                (Bin "++" (Var "xs") (ListOf (Var "x")))
              ]

main :: IO()
main = do
    runAgda test
    runIdris test
    runLean test
    runRocq test