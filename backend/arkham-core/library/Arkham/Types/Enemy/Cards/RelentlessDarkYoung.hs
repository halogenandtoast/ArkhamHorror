{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy.Cards.RelentlessDarkYoung where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.EnemyId
import Arkham.Types.GameValue
import Arkham.Types.Message
import Arkham.Types.Prey
import Arkham.Types.SkillType
import ClassyPrelude
import Lens.Micro

newtype RelentlessDarkYoung = RelentlessDarkYoung Attrs
  deriving newtype (Show, ToJSON, FromJSON)

relentlessDarkYoung :: EnemyId -> RelentlessDarkYoung
relentlessDarkYoung uuid =
  RelentlessDarkYoung
    $ baseAttrs uuid "01179"
    $ (healthDamage .~ 2)
    . (sanityDamage .~ 1)
    . (fight .~ 4)
    . (health .~ Static 5)
    . (evade .~ 2)
    . (prey .~ LowestSkill SkillAgility)

instance HasModifiersFor env RelentlessDarkYoung where
  getModifiersFor _ _ _ = pure []

instance HasModifiers env RelentlessDarkYoung where
  getModifiers _ (RelentlessDarkYoung Attrs {..}) =
    pure . concat . toList $ enemyModifiers

instance ActionRunner env => HasActions env RelentlessDarkYoung where
  getActions i window (RelentlessDarkYoung attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env RelentlessDarkYoung where
  runMessage msg (RelentlessDarkYoung attrs) = case msg of
    EndRound -> do
      pure $ RelentlessDarkYoung $ attrs & damage %~ max 0 . subtract 2
    _ -> RelentlessDarkYoung <$> runMessage msg attrs
