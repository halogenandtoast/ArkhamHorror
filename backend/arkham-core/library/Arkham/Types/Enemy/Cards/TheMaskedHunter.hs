{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy.Cards.TheMaskedHunter where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.EnemyId
import Arkham.Types.GameValue
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Prey
import Arkham.Types.Query
import Arkham.Types.Source
import Arkham.Types.Target
import ClassyPrelude
import Lens.Micro

newtype TheMaskedHunter = TheMaskedHunter Attrs
  deriving newtype (Show, ToJSON, FromJSON)

theMaskedHunter :: EnemyId -> TheMaskedHunter
theMaskedHunter uuid = TheMaskedHunter $ (baseAttrs uuid "01121b")
  { enemyHealthDamage = 2
  , enemySanityDamage = 1
  , enemyFight = 4
  , enemyHealth = Static 4
  , enemyEvade = 2
  , enemyPrey = MostClues
  }

instance (IsInvestigator investigator) => HasActions env investigator TheMaskedHunter where
  getActions i window (TheMaskedHunter attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env TheMaskedHunter where
  runMessage msg (TheMaskedHunter attrs@Attrs {..}) = case msg of
    EnemySpawnEngagedWithPrey eid | eid == enemyId -> do
      playerCount <- unPlayerCount <$> asks (getCount ())
      TheMaskedHunter
        <$> runMessage msg (attrs & health %~ fmap (+ (2 * playerCount)))
    EnemyEngageInvestigator eid iid | eid == enemyId -> do
      unshiftMessages
        [ AddModifier
          (InvestigatorTarget iid)
          (CannotDiscoverClues (EnemySource eid))
        , AddModifier
          (InvestigatorTarget iid)
          (CannotSpendClues (EnemySource eid))
        ]
      TheMaskedHunter <$> runMessage msg attrs
    _ -> TheMaskedHunter <$> runMessage msg attrs
