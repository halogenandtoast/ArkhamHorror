{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy.Cards.TheMaskedHunter where

import Arkham.Import

import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.Prey

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

instance HasModifiersFor env TheMaskedHunter where
  getModifiersFor _ (InvestigatorTarget iid) (TheMaskedHunter Attrs {..}) = do
    if iid `elem` enemyEngagedInvestigators
      then pure [CannotDiscoverClues, CannotSpendClues]
      else pure []
  getModifiersFor _ _ _ = pure []

instance HasModifiers env TheMaskedHunter where
  getModifiers _ (TheMaskedHunter Attrs {..}) =
    pure . concat . toList $ enemyModifiers

instance ActionRunner env => HasActions env TheMaskedHunter where
  getActions i window (TheMaskedHunter attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env TheMaskedHunter where
  runMessage msg (TheMaskedHunter attrs@Attrs {..}) = case msg of
    EnemySpawnEngagedWithPrey eid | eid == enemyId -> do
      playerCount <- unPlayerCount <$> asks (getCount ())
      TheMaskedHunter
        <$> runMessage msg (attrs & health %~ fmap (+ (2 * playerCount)))
    _ -> TheMaskedHunter <$> runMessage msg attrs
