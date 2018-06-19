module Parser (parseProgram) where
import Control.Monad
import Control.Applicative hiding ((<|>))
import qualified Data.List as List
import Text.Parsec.String
import Text.Parsec
import Ast

lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

tok :: String -> Parser String
tok = lexeme . string

($>) :: (Applicative f) => a -> f b -> f a
a $> f = const a <$> f

op :: (a -> a -> a) -> String -> Parser (a -> a -> a)
op f str = tok str >> return f

anyOrder :: Parser a -> Parser a -> Parser (a, a)
anyOrder a b =   try $ (,) <$> a <*> (tok "," *> b)
             <|> (flip (,)) <$> a <*> (tok "," *> b)

pIdent :: Parser String
pIdent = lexeme (many1 letter)

pNat :: Parser Expr
pNat = fmap (ENat . read) $ lexeme (many1 digit)

pVar :: Parser Expr
pVar = fmap EVar pIdent

pBool :: Parser Expr
pBool = fmap f $ try (tok "true") <|> tok "false"
  where f "true"  = EBool True
        f "false" = EBool False

pFst :: Parser Expr
pFst = fmap EFst $ tok "fst" >> tok "(" *> pExp <* tok ")"

pSnd :: Parser Expr
pSnd = fmap ESnd $ tok "snd" >> tok "(" *> pExp <* tok ")"

pLam :: Parser Expr
pLam = do
  tok "\\"
  v <- pIdent
  tok ":"
  t <- pType
  tok "."
  e <- pExp
  return $ ELam v t e

pInl :: Parser Expr
pInl = do
  tok "inl" >> tok "("
  e <- pExp
  tok ")" >> tok ":"
  t <- pType
  return $ EInl e t

pInr :: Parser Expr
pInr = do
  tok "inr" >> tok "("
  e <- pExp
  tok ")" >> tok ":"
  t <- pType
  return $ EInr e t

pPair :: Parser Expr
pPair = EPair <$> (tok "(" >> pExp <* tok ",") <*> (pExp <* tok ")")

pCaseInl :: Parser (String, Expr)
pCaseInl = do
  tok "inl"
  v <- pIdent
  tok "=>"
  e <- pExp
  return (v, e)

pCaseInr :: Parser (String, Expr)
pCaseInr = do
  tok "inr"
  v <- pIdent
  tok "=>"
  e <- pExp
  return (v, e)

-- ECase    Expr String Expr String Expr
pCase :: Parser Expr
pCase = do
  tok "case"
  e <- pExpPrim
  tok "of"
  ((lv, le), (rv, re)) <- anyOrder pCaseInl pCaseInr
  return $ ECase e lv le rv re

pExpPrim :: Parser Expr
pExpPrim = try pNat
     <|> try pBool
     <|> try pFst
     <|> try pSnd
     <|> try pLam
     <|> try pInl
     <|> try pInr
     <|> try pCase
     <|> try pVar
     <|> try pPair
     <|> (tok "(" >> pExp) <* tok ")"

pExp = fmap (foldl1 EApp) $ many1 pExpPrim

pProg = pExp <* eof

pType :: Parser ExprType
pType = chainr1 pSumType (op TFun "->")
  where
    pTypeAtom = try (TBool $> tok "bool") 
              <|> try (TNat $> tok "nat")
              <|> (tok "(" *> pType <* tok ")")
    pProdType = chainl1 pTypeAtom (op TProd "*")
    pSumType = chainl1 pProdType (op TSum "+")

parseProgram :: String -> Either ParseError Expr
parseProgram s = parse pProg "" s
