module BetaReduceTest where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Lib (Expr(Lit, Term, Abs, App), betaReduce)

tests :: TestTree
tests =
  testGroup "betaReduce"
    [ testCase "Application (identity)" $
        betaReduce [] expr_app @?= Just (Lit "*")
    , testCase "Application (const)" $
        betaReduce [] expr_const @?= Just (Lit "*")
    , testCase "Inner expr" $
        betaReduce [] inner_expr @?= Just (Abs "y" (Lit "*"))
    , testCase "App abs argument" $
        betaReduce [] app_abs_expr @?= Just (Abs "z" (Lit "*"))
    , testCase "Abstraction applied to abstraction" $
        betaReduce [] abs_abs_expr @?= Just id'
    ]
  where
    const' = Abs "x" (Abs "y" (Term "x"))
    id' = Abs "x" (Term "x")

    expr_app = App id' (Lit "*")
    expr_const = App (App (App const' (App const' (Lit "*"))) (Lit "!")) (Lit "!!")
    inner_expr = App const' (Lit "*")
    app_abs_expr = App id' (Abs "z" (App id' (Lit "*")))
    abs_abs_expr = App id' id'
