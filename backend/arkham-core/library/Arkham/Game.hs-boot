{-# OPTIONS_GHC -Wno-orphans #-}
module Arkham.Game where

import Arkham.Asset.Attrs
import Arkham.Classes.HasTokenValue
import Arkham.Classes.HasModifiersFor
import Arkham.Classes.Query
import Arkham.Enemy.Attrs
import Arkham.Id
import Arkham.Investigator.Attrs
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Projection
import Arkham.SkillTest.Base
import Control.Monad.Random

data Game

class HasGameRef a where
  gameRefL :: Lens' a (IORef Game)

class HasGame m where
  getGame :: m Game

class HasStdGen a where
  genL :: Lens' a (IORef StdGen)

instance HasModifiersFor ()

instance Query ExtendedCardMatcher
instance Query InvestigatorMatcher
instance Query AssetMatcher
instance Query LocationMatcher

instance Projection InvestigatorAttrs
instance Projection EnemyAttrs
instance Projection AssetAttrs

instance HasTokenValue InvestigatorId

gameSkillTest :: Game -> Maybe SkillTest
