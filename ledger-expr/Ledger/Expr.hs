module Ledger.Expr where

{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

data ExprF  :: (* -> *) -> * -> * where
  Const :: Int                    -> ExprF r Int
  Add   :: r Int  -> r Int        -> ExprF r Int
  Mul   :: r Int  -> r Int        -> ExprF r Int
  Cond  :: r Bool -> r a  -> r a  -> ExprF r a
  IsEq  :: r Int  -> r Int        -> ExprF r Bool