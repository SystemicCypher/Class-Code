module Typechecker (Env, TypeError, typeOf, defaultEnv) where

import Ast
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Applicative

import Data.Either

-- An environment is a mapping from a variable name to a type
type Env = Map String ExprType

-- The default environment already contains a few function signatures
-- that you can play around with. Do *not* alter this.
defaultEnv = Map.fromList [
    ("add", TFun TNat (TFun TNat TNat)),
    ("even", TFun TNat TBool),
    ("const", TFun TNat (TFun TNat TNat)),
    ("twice", TFun (TFun TNat TNat) (TFun TNat TNat)),
    ("n2b", TFun TNat TBool),
    ("b2n", TFun TBool TNat)
  ]

-- An error is simply a string describing the error
type TypeError = String

-- A helper function you might need, transforms a Maybe a instance into a Either b a istance,
-- given a `b` that represents failure.
-- Example: if Map.lookup returns Maybe ExprType, then you can use toEither like this:
-- toEtiher "Variable not in scope" (Map.lookup "x" env)
toEither :: b -> Maybe a -> Either b a
toEither err Nothing = Left err
toEither _ (Just a)  = Right a

-- typeOf is the function you need to complete. The first argument is an expression
-- which we want to typecheck. The second is the typing environment.
-- The result of the function should be:
--   (Left errorMsg) if the expression doesn't type check
--   (Right type)    if the expression does type check
typeOf :: Expr -> Env -> Either TypeError ExprType
--Base types
typeOf (EVar name) en =  toEither ("Variable " ++ name ++ " not in scope. Unknown type.")  (Map.lookup name en)
typeOf (ENat value) _ = Right TNat
typeOf (EBool bool) _ = Right TBool

--Lambdas - checks if types valid
typeOf (ELam name parameterType expr) en = 
    if isRight(typeOf expr (Map.insert name parameterType en))
        then Right (TFun parameterType (fromRight TNat (typeOf expr (Map.insert name parameterType en))))
        else Left "Invalid type."

--Applies to functions - checks if type applied matches type of parameters (A union in a sense)
typeOf (EApp exp1 exp2) en =
    if isRight(typeOf exp1 en) &&  isRight(typeOf exp2 en)
        then Right (TSum (fromRight TNat (typeOf exp1 en)) (fromRight TNat (typeOf exp2 en)))
        else Left "Type mismatch."


--Assigns pairs - checks if types are valid
typeOf (EPair exp1 exp2) en = 
    if isRight(typeOf exp1 en) &&  isRight(typeOf exp2 en)
        then Right (TProd (fromRight TNat (typeOf exp1 en)) (fromRight TNat (typeOf exp2 en)))
        else Left "Invalid pair."


typeOf (EFst (EVar name)) en = toEither ("Variable " ++ name ++ " not in scope. Unknown type.")  (Map.lookup name en)
typeOf (EFst ENat) en = Right TNat
typeOf (EFst EBool) en = Right TBool
    EBool -> 
    EApp  -> 
    ELam  -> 
    EPair -> 
    EFst  -> 
    ESnd  -> 
    EInl  -> 
    EInr  -> 
    ECase -> 

typeOf (ESnd expr) en = typeOf expr en

--Cases! Need union types
typeOf (ECase exp1 str1 exp2 str2 exp3) en = 
    if isRight(typeOf exp1 en) && isRight(typeOf exp2 en) && isRight(typeOf exp3 en)
        then Right (fromRight TNat (typeOf exp1 en))
        else Left "Type mismatch."

typeOf (EInl expr expTy) en =
    if isRight(typeOf expr en) && (fromRight TNat (typeOf expr en)) == expTy
        then Right expTy
        else Left "Type mismatch."

typeOf (EInr expr expTy) en = 
    if isRight(typeOf expr en) && (fromRight TNat (typeOf expr en)) == expTy
        then Right expTy
        else Left "Type mismatch."
