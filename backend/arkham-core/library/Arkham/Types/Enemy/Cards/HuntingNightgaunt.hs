{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy.Cards.HuntingNightgaunt where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.EnemyId
import Arkham.Types.GameValue
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Source
import ClassyPrelude

newtype HuntingNightgaunt = HuntingNightgaunt Attrs
  deriving newtype (Show, ToJSON, FromJSON)

huntingNightgaunt :: EnemyId -> HuntingNightgaunt
huntingNightgaunt uuid = HuntingNightgaunt $ (baseAttrs uuid "01172")
  { enemyHealthDamage = 1
  , enemySanityDamage = 1
  , enemyFight = 3
  , enemyHealth = Static 4
  , enemyEvade = 1
  }

instance (IsInvestigator investigator) => HasActions investigator HuntingNightgaunt where
  getActions i (HuntingNightgaunt attrs) = getActions i attrs

instance (EnemyRunner env) => RunMessage env HuntingNightgaunt where
  runMessage msg (HuntingNightgaunt attrs@Attrs {..}) = case msg of
    TryEvadeEnemy iid eid skillType onSuccess onFailure skillTestModifiers tokenResponses
      | eid == enemyId
      -> HuntingNightgaunt
        <$> runMessage
              (TryEvadeEnemy
                iid
                eid
                skillType
                onSuccess
                onFailure
                (DoubleNegativeModifiersOnTokens (EnemySource eid)
                : skillTestModifiers
                )
                tokenResponses
              )
              attrs
    _ -> HuntingNightgaunt <$> runMessage msg attrs
