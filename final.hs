{-# LANGUAGE GADTs,FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

data CJPTY where
    Num :: Int -> CJPTY
    Boolean :: Bool -> CJPTY
    Id:: String -> CJPTY
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
    Fix :: CJPTY -> CJPTY
    deriving (Show, Eq)

data CJPTYtype where
    TNum :: CJPTYtype
    TBool :: CJPTYtype
    (:->:) :: CJPTYtype -> CJPTYtype -> CJPTYtype
    deriving (Show, Eq)


data CJPTYVal where
    NumV :: Int -> CJPTYVal
    BooleanV :: Bool -> CJPTYVal
    ClosureV :: String -> CJPTY -> Env -> CJPTYVal
    deriving (Show,Eq)

type Env = [(String, CJPTYVal)]
type Cont = [(String, CJPTYtype)]

-- Part 1 ( Using typeof monad ) -- 

typeOf :: Cont -> CJPTY -> Maybe CJPTYtype

typeOf _ ( Num n ) = if n<0 then Nothing else return TNum

typeOf _ ( Boolean b ) = return TBool

typeOf g (Id x) = (lookup x g)

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
typeOf g (Fix t) = do{
    (d :->: r) <- typeOf g t;
    return r;
}


--Part2: Evaluation

eval :: Env -> CJPTY -> Maybe CJPTYVal

eval _ (Num n) = return (NumV n)

eval _ (Boolean b) = return (BooleanV b)

eval env (Id x) = (lookup x env)

eval env (Plus l r) = do
  NumV n1 <- eval env l
  NumV n2 <- eval env r
  return (NumV (n1 + n2))

eval env (Minus l r) = do
  NumV n1 <- eval env l
  NumV n2 <- eval env r
  return (NumV (n1 - n2))

eval env (Mult l r) = do
  NumV n1 <- eval env l
  NumV n2 <- eval env r
  return (NumV (n1 * n2))

eval env (Div l r) = do
  NumV n1 <- eval env l
  NumV n2 <- eval env r
  if n2 == 0
    then Nothing
    else return (NumV (n1 `div` n2))

eval env (Exp l r) = do
  NumV n1 <- eval env l
  NumV n2 <- eval env r
  return (NumV (n1 ^ n2))

eval env (Between n l r) = do
  NumV n' <- eval env n
  NumV l' <- eval env l
  NumV r' <- eval env r
  return (BooleanV (n' >= l' && n' <= r'))

eval env (Lambda i t b ) = Just (ClosureV i b env)

eval env (App f a) = do
  (ClosureV i b env') <- eval env f
  argVal <- eval env a
  eval ((i, argVal):env') b

eval env (Bind x v body) = do
  val <- eval env v
  eval ((x, val):env) body

eval env (If c t e) = do
  BooleanV b <- eval env c
  if b
    then eval env t
    else eval env e

eval env (And l r) = do
  BooleanV b1 <- eval env l
  BooleanV b2 <- eval env r
  return (BooleanV (b1 && b2))

eval env (Or l r) = do
  BooleanV b1 <- eval env l
  BooleanV b2 <- eval env r
  return (BooleanV (b1 || b2))

eval env (Leq l r) = do
  NumV n1 <- eval env l
  NumV n2 <- eval env r
  return (BooleanV (n1 <= n2))

eval env (IsZero n) = do
  NumV n' <- eval env n
  return (BooleanV (n' == 0))

eval env (Fix f) = do{
    (ClosureV i b env') <- (eval env f);
    eval env' (subst i (Fix (Lambda i (TNum) b)) b)
}

--prat3

subst:: String -> CJPTY -> CJPTY -> CJPTY
subst i v (Num x) = (Num x)
subst i v (Boolean x) = (Boolean x)
subst i v (Id i') = if i == i' then v else (Id i') 
subst i v (Plus x y) = (Plus (subst i v x) (subst i v y))
subst i v (Minus x y) = (Minus (subst i v x) (subst i v y))
subst i v (Mult x y) = (Mult (subst i v x) (subst i v y))
subst i v (Div x y) = (Div (subst i v x) (subst i v y))
subst i v (Exp x y) = (Exp (subst i v x) (subst i v y))
subst i v (Between x y z) = (Between (subst i v x) (subst i v y) (subst i v z))
subst i v (And x y) = (And (subst i v x) (subst i v y))
subst i v (Or x y) = (Or (subst i v x) (subst i v y))
subst i v (Leq x y) = (Leq (subst i v x) (subst i v y))
subst i v (IsZero x) = (IsZero (subst i v x))
subst i v (If x y z) = (If (subst i v x) (subst i v y) (subst i v z))
subst i v (Bind i' v' b) = if i == i' then (Bind i' (subst i v v') b)
 else (Bind i' (subst i v v') (subst i v b))
subst i v (Lambda i' t b) = (Lambda i' t (subst i v b))
subst i v (Fix f) = (Fix (subst i v f))


-- A closure is a lambda expression with an environment
--data Closure = Closure String CJPTY Env

-- We represent evaluation failures with a Maybe type
-- (e.g., division by zero or application of a non-closure)
