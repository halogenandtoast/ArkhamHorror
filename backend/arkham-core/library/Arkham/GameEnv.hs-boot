module Arkham.GameEnv where

import Arkham.Prelude

import Arkham.SkillTest.Base

data GameEnv

newtype GameT a = GameT { unGameT :: ReaderT GameEnv IO a }

instance Functor GameT
instance Applicative GameT
instance Monad GameT
instance MonadIO GameT
instance MonadRandom GameT

getSkillTest :: GameT (Maybe SkillTest)
