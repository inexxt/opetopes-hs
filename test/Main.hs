module Main where

-- import Test.HUnit
import Test.Tasty
import Test.Tasty.HUnit

import Opetope

a = Point "a"
b = Point "b"
ab1 = Arrow "ab1" a b
ab2 = Arrow "ab1" a b
alpha = Face "alpha" [ab1] ab2

test_cod_arrow = testCase "ab_cod" (assertEqual "ab_cod" (cod ab1) b)
test_dom_arrow = testCase "ab_dom" (assertEqual "ab_dom" (dom ab1) [a])

test_cod_face = testCase "alpha_cod" (assertEqual "alpha_cod" (cod alpha) ab2)
test_dom_face = testCase "alpha_dom" (assertEqual "alpha_dom" (dom alpha) [ab1])

test_match_alpha = testCase "test_match_alpha" (assertBool "test_match_alpha" (match [ab1] ab2))

test_trivial = testCase "aaa" (assertBool "aaa" True)

main :: IO ()
main = do 
    defaultMain (testGroup "Our Library Tests" 
        [test_trivial,
        test_cod_arrow,
        test_dom_arrow,
        test_cod_face,
        test_dom_face,
        test_match_alpha])