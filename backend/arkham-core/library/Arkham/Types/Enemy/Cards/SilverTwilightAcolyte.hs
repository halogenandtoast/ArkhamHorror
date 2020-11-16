{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy.Cards.SilverTwilightAcolyte where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.EnemyId
import Arkham.Types.GameValue
import Arkham.Types.Message
import Arkham.Types.Prey
import ClassyPrelude

newtype SilverTwilightAcolyte = SilverTwilightAcolyte Attrs
  deriving newtype (Show, ToJSON, FromJSON)

silverTwilightAcolyte :: EnemyId -> SilverTwilightAcolyte
silverTwilightAcolyte uuid =
  SilverTwilightAcolyte $ (weaknessBaseAttrs uuid "01102")
    { enemyHealthDamage = 1
    , enemySanityDamage = 0
    , enemyFight = 2
    , enemyHealth = Static 3
    , enemyEvade = 3
    , enemyPrey = SetToBearer
    }

instance HasModifiersFor env SilverTwilightAcolyte where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env SilverTwilightAcolyte where
  getActions i window (SilverTwilightAcolyte attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env SilverTwilightAcolyte where
  runMessage msg (SilverTwilightAcolyte attrs@Attrs {..}) = case msg of
    EnemyAttack _ eid | eid == enemyId -> do
      unshiftMessage PlaceDoomOnAgenda
      SilverTwilightAcolyte <$> runMessage msg attrs
    _ -> SilverTwilightAcolyte <$> runMessage msg attrs
