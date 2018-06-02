module EtaReductionTest where

import           Lib              (ErrIdentifier(ErrIdentifier),
                                   Expr (Abs, App, ErrAbs, Lit, Term),
                                   etaReduce, isFreeVarOf)
import           Test.Tasty       (TestTree, testGroup)
import           Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup "Eta Reduction"
    [testGroup "isFreeVarOf" 
      [ testCase "x in x" $ isFreeVarOf "x" x @?= True
      , testCase "x in λx.x" $ isFreeVarOf "x" (Abs "x" x) @?= False
      , testCase "y in λx.xy" $ isFreeVarOf "y" (Abs "x" $ App x y) @?= True
      , testCase "y in λx.x" $ isFreeVarOf "y" (Abs "x" x) @?= False
      , testCase "literal" $ isFreeVarOf "x" (Lit "x") @?= False
      , testCase "erorr" $ isFreeVarOf "x" (ErrAbs (ErrIdentifier "x") x) @?= False
      ]
    , testGroup "etaReduce"
      [ testCase "λx.fx -> f" $ etaReduce (Abs "x" $ App y x) @?= y
      , testCase "λx.xx" $ etaReduce (Abs "x" $ App x x) @?= (Abs "x" $ App x x)
      ]
    ]

  where
    x = Term "x"
    y = Term "y"
