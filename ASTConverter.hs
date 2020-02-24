module ASTConverter where

import AbsAssignment
import qualified AST as A 


transIdent :: Ident -> String 
transIdent x = case x of
  Ident string -> string

transProg :: Prog -> A.Prog String String
transProg x = case x of
  PROG funs -> A.Prog $ map transFun funs 

transFun :: Fun -> A.Fun String String
transFun x = case x of
  FUNS ident idents exp -> 
      A.Fun (
             transIdent ident,
             map transIdent idents,
             transExp exp
           )

transExp :: Exp -> A.Exp String String
transExp x = case x of
  EXP_ADD exp term ->
      A.ADD (transExp exp)
            (transTerm term)   

  EXP_SUB exp term -> 
      A.SUB (transExp exp)
            (transTerm term)

  EXP_Term term -> 
      transTerm term

transTerm :: Term -> A.Exp String String
transTerm x = case x of
  TERM_MUL term factor -> 
      A.MUL (transTerm term)
            (transFactor factor)

  TERM_DIV term factor -> 
      A.DIV (transTerm term)
            (transFactor factor)

  TERM_FACT factor ->
      transFactor factor

transFactor :: Factor -> A.Exp String String
transFactor x = case x of
  FACT_CONST integer -> 
      A.CONST (fromInteger integer::Int)

  FACT_VAR ident -> 
      A.VAR (transIdent ident)

  FACT_NEG exp -> 
      A.NEG (transExp exp)

  FACT_APP ident exps -> 
      A.APP (transIdent ident)
            (map transExp exps)

  FACT_LET funs exp -> 
      A.LET (map transFun funs)
            (transExp exp)

  EXP_COND bexp exp1 exp2 -> 
      A.COND (transBExp bexp)
             (transExp exp1)
             (transExp exp2)

transBExp :: BExp -> A.BExp String String
transBExp x = case x of
  BEXP_AND bexp bterm -> 
      A.AND (transBExp bexp)
            (transBTerm bterm)

  BEXP_OR bexp bterm -> 
      A.OR (transBExp bexp)
           (transBTerm bterm)

  BEXP_TERM bterm -> 
      transBTerm bterm

transBTerm :: BTerm -> A.BExp String String
transBTerm x = case x of
  BTERM_GT exp1 exp2 -> 
      A.Gt (transExp exp1)  
           (transExp exp2)

  BTERM_LT exp1 exp2 -> 
      A.Gt (transExp exp1)  
           (transExp exp2)

  BTERM_EQ exp1 exp2 -> 
      A.Eq (transExp exp1)  
           (transExp exp2)

  BTERM_NOT bexp -> 
      A.NOT (transBExp bexp)

  BTERM_TRUE -> 
      A.TRUE 

  BTERM_FALSE -> 
      A.FALSE 

