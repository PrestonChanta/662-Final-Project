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
    Lambda :: String -> CJPTY -> CJPTY
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

-- Lambda --

-- App --

-- Bind -- 

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
