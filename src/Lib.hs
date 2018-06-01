module Lib where

type Identifier = String

type Env = [(Identifier, Value)]

data Expr
  = Lit String
  | Term Identifier
  | Abs Identifier Expr
  | App Expr Expr
  deriving (Show, Eq)

data Value
  = Value String
  | Closure Expr Env Identifier
  deriving (Show, Eq)

eval :: Env -> Expr -> Maybe Value
eval _   (Lit string)          = Just $ Value string
eval env (Term identifier)     = lookup identifier env
eval env (Abs identifier expr) = Just $ Closure expr env identifier
eval env (App t u) =
  let et = eval env t
      eu = eval env u
   in case et of
        Just (Closure expr env' arg) ->
          case eu of
            Nothing    -> Nothing
            Just value -> eval ((arg, value) : env') expr
        _                            -> Nothing

checkShadowing :: [Identifier] -> Expr -> [Identifier]
checkShadowing args (Abs arg expr) 
  | arg !! 0 == '_' = nested
  | elem arg args   = arg : nested
  | otherwise       = nested
    where 
      nested  = checkShadowing (arg : args) expr
checkShadowing args (App t u) = concatMap (checkShadowing args) [t, u]
checkShadowing _ _ = []
