{-# LANGUAGE GADTs,FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

data CJPTY where
    Num :: Int -> CJPTY
    Boolean :: Bool -> CJPTY
    Plus :: CJPTY -> CJPTY -> CJPTY
    Minus :: CJPTY -> CJPTY -> CJPTY
    Mult :: CJPTY -> CJPTY -> CJPTY
    Div :: CJPTY -> CJPTY -> CJPTY
    Exp :: CJPTY -> CJPTY -> CJPTY
    Between :: CJPTY -> CJPTY -> CJPTY -> CJPTY
    Lambda :: String -> CJPTYtype -> CJPTY -> CJPTY
    App :: CJPTY -> CJPTY -> CJPTY
    Bind :: String -> CJPTY -> CJPTY -> CJPTY
    If :: CJPTY -> CJPTY -> CJPTY -> CJPTY
    And :: CJPTY -> CJPTY -> CJPTY 
    Or :: CJPTY -> CJPTY -> CJPTY  
    Leq :: CJPTY -> CJPTY -> CJPTY  
    IsZero :: CJPTY -> CJPTY
    deriving (Show, Eq)

data CJPTYtype where
    TNum :: CJPTYtype
    TBool :: CJPTYtype
    (:->:) :: CJPTYtype -> CJPTYtype -> CJPTYtype
    deriving (Show, Eq)

type Env = [(String, CJPTY)]

type Cont = [(String, CJPTYtype)]

-- Part 1 ( Using typeof monad ) -- 

typeOf :: Cont -> CJPTY -> Maybe CJPTYtype

typeOf _ ( Num n ) = if n<0 then Nothing else return TNum

typeOf _ ( Boolean b ) = return TBool

typeOf g ( Plus l r ) = do { TNum <- typeOf g l;
                             TNum <- typeOf g r;
                             return TNum; }

typeOf g ( Minus l r ) = do { TNum <- typeOf g l;
                              TNum <- typeOf g r;
                              return TNum; }

typeOf g ( Mult l r ) = do { TNum <- typeOf g l;
                             TNum <- typeOf g r;
                             return TNum; }

typeOf g ( Div l r ) = do { TNum <- typeOf g l;
                            TNum <- typeOf g r;
                            return TNum; }

typeOf g ( Exp l r ) = do { TNum <- typeOf g l;
                            TNum <- typeOf g r;
                            return TNum; }   

typeOf g ( Between n l r ) = do { TNum <- typeOf g n;
                                  TNum <- typeOf g l;
                                  TNum <- typeOf g r;
                                  return TBool; }    

typeOf g ( Lambda i d b ) = do { r <- typeOf ((i,d):g) b;
                                 return (d :->: r )}

typeOf g ( App f a ) = do { a' <- typeOf g a;
                            d :->: r <- typeOf g f;
                            if a' == d then return r else Nothing}

typeOf g ( Bind i v b ) = do { tv <- typeOf g v;
                               typeOf ((i,tv):g) b }

typeOf g ( If c t e ) = do { TBool <- typeOf g c;
                             t' <- typeOf g t;
                             e' <- typeOf g e;
                             if t'==e'; then return t' else Nothing }

typeOf g ( And l r ) = do { TBool <- typeOf g l;
                            TBool <- typeOf g r;
                            return TBool; }

typeOf g ( Or l r ) = do { TBool <- typeOf g l;
                           TBool <- typeOf g r;
                           return TBool; }

typeOf g ( Leq l r ) = do { TBool <- typeOf g l;
                            TBool <- typeOf g r;
                            return TBool; }

typeOf g ( IsZero n ) = do { TNum <- typeOf g n;
                            return TBool; }


--Part2: Evaluation

eval :: Env -> CJPTY -> Maybe CJPTY

eval _ (Num n) = return (Num n)

eval _ (Boolean b) = return (Boolean b)

eval env (Plus l r) = do
  Num n1 <- eval env l
  Num n2 <- eval env r
  return (Num (n1 + n2))

eval env (Minus l r) = do
  Num n1 <- eval env l
  Num n2 <- eval env r
  return (Num (n1 - n2))

eval env (Mult l r) = do
  Num n1 <- eval env l
  Num n2 <- eval env r
  return (Num (n1 * n2))

eval env (Div l r) = do
  Num n1 <- eval env l
  Num n2 <- eval env r
  if n2 == 0
    then Nothing
    else return (Num (n1 `div` n2))

eval env (Exp l r) = do
  Num n1 <- eval env l
  Num n2 <- eval env r
  return (Num (n1 ^ n2))

eval env (Between n l r) = do
  Num n' <- eval env n
  Num l' <- eval env l
  Num r' <- eval env r
  return (Boolean (n' >= l' && n' <= r'))

eval env (Lambda arg ty body) = return (Closure arg body env)

eval env (App f a) = do
  Closure arg body env' <- eval env f
  argVal <- eval env a
  eval ((arg, argVal):env') body

eval env (Bind x v body) = do
  val <- eval env v
  eval ((x, val):env) body

eval env (If c t e) = do
  Boolean b <- eval env c
  if b
    then eval env t
    else eval env e

eval env (And l r) = do
  Boolean b1 <- eval env l
  Boolean b2 <- eval env r
  return (Boolean (b1 && b2))

eval env (Or l r) = do
  Boolean b1 <- eval env l
  Boolean b2 <- eval env r
  return (Boolean (b1 || b2))

eval env (Leq l r) = do
  Num n1 <- eval env l
  Num n2 <- eval env r
  return (Boolean (n1 <= n2))

eval env (IsZero n) = do
  Num n' <- eval env n
  return (Boolean (n' == 0))

-- A closure is a lambda expression with an environment
data Closure = Closure String CJPTY Env

-- We represent evaluation failures with a Maybe type
-- (e.g., division by zero or application of a non-closure)
