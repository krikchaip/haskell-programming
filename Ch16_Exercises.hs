{-# LANGUAGE FlexibleInstances #-}

module Ch16_Exercises where

-- Write Functor instances for the following datatypes.

-- 01
data Quant a b = Finance | Desk a | Bloor b deriving (Show, Eq)

instance Functor (Quant a) where
  _ `fmap` Finance   = Finance
  _ `fmap` (Desk a)  = Desk a
  f `fmap` (Bloor b) = Bloor $ f b

-- 02
data K a b = K a deriving (Show, Eq)

instance Functor (K a) where
  _ `fmap` (K a) = K a

-- 03
data Flip f b a = Flip (f a b) deriving (Show, Eq)

instance Functor (Flip K b) where
  f `fmap` (Flip (K a)) = Flip $ K (f a)

-- 04
data EvilGoateeConst a b = GoatyConst b deriving (Show, Eq)

instance Functor (EvilGoateeConst a) where
  f `fmap` (GoatyConst b) = GoatyConst (f b)

-- 05
data LiftItOut f a = LiftItOut (f a) deriving (Show, Eq)

instance Functor f => Functor (LiftItOut f) where
  f `fmap` (LiftItOut fa) = LiftItOut $ f <$> fa

-- 06
data Parappa f g a = DaWrappa (f a) (g a) deriving (Show, Eq)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  f `fmap` (DaWrappa fa ga) = DaWrappa (f <$> fa) (f <$> ga)

-- 07
data IgnoreOne f g a b = IgnoringSomething (f a) (g b) deriving (Show, Eq)

instance Functor g => Functor (IgnoreOne f g a) where
  f `fmap` (IgnoringSomething fa gb) = IgnoringSomething fa (f <$> gb)

-- 08
data Notorious g o a t = Notorious (g o) (g a) (g t) deriving (Show, Eq)

instance Functor g => Functor (Notorious g o a) where
  f `fmap` (Notorious go ga gt) = Notorious go ga (f <$> gt)

-- 09
data List a = Nil | Cons a (List a) deriving (Show, Eq)

instance Functor List where
  _ `fmap` Nil         = Nil
  f `fmap` (Cons a la) = Cons (f a) (f `fmap` la)

-- 10
data GoatLord a =
    NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a)
              (GoatLord a)
              (GoatLord a)
  deriving (Show, Eq)

instance Functor GoatLord where
  _ `fmap` NoGoat                  = NoGoat
  f `fmap` (OneGoat a)             = OneGoat (f a)
  f `fmap` (MoreGoats ga ga' ga'') = MoreGoats (f `fmap` ga)
                                               (f `fmap` ga')
                                               (f `fmap` ga'')

-- 11
data TalkToMe a = Halt | Print String a | Read (String -> a)

instance Functor TalkToMe where
  _ `fmap` Halt        = Halt
  f `fmap` (Print s a) = Print s (f a)
  f `fmap` Read g      = Read (f . g)
