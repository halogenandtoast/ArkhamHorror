{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy.Cards.OBannionsThug where

import Arkham.Import

import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Enemy.Runner

newtype OBannionsThug = OBannionsThug Attrs
  deriving newtype (Show, ToJSON, FromJSON)

oBannionsThug :: EnemyId -> OBannionsThug
oBannionsThug uuid = OBannionsThug $ (weaknessBaseAttrs uuid "02097")
  { enemyHealthDamage = 2
  , enemySanityDamage = 0
  , enemyFight = 4
  , enemyHealth = Static 2
  , enemyEvade = 2
  }

instance HasModifiersFor env OBannionsThug where
  getModifiersFor _ (InvestigatorTarget iid) (OBannionsThug a@Attrs {..})
    | iid `elem` enemyEngagedInvestigators = pure
    $ toModifiers a [CannotGainResources]
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env OBannionsThug where
  getActions i window (OBannionsThug attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env OBannionsThug where
  runMessage msg (OBannionsThug attrs) = OBannionsThug <$> runMessage msg attrs
