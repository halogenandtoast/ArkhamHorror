module Arkham.Types.Enemy.Cards.TheMaskedHunter where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Enemy.Runner
import Arkham.Types.GameValue
import Arkham.Types.Modifier
import Arkham.Types.Prey
import Arkham.Types.Query
import Arkham.Types.Target

newtype TheMaskedHunter = TheMaskedHunter EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theMaskedHunter :: EnemyCard TheMaskedHunter
theMaskedHunter = enemy TheMaskedHunter Cards.theMaskedHunter
  $ (healthDamageL .~ 2)
  . (sanityDamageL .~ 1)
  . (fightL .~ 4)
  . (healthL .~ Static 4)
  . (evadeL .~ 2)
  . (preyL .~ MostClues)

instance HasCount PlayerCount env () => HasModifiersFor env TheMaskedHunter where
  getModifiersFor _ target (TheMaskedHunter a) | isTarget a target = do
    healthModifier <- getPlayerCountValue (PerPlayer 2)
    pure $ toModifiers a [HealthModifier healthModifier]
  getModifiersFor _ (InvestigatorTarget iid) (TheMaskedHunter a@EnemyAttrs {..})
    = if iid `elem` enemyEngagedInvestigators
      then pure $ toModifiers a [CannotDiscoverClues, CannotSpendClues]
      else pure []
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env TheMaskedHunter where
  getActions i window (TheMaskedHunter attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env TheMaskedHunter where
  runMessage msg (TheMaskedHunter attrs) =
    TheMaskedHunter <$> runMessage msg attrs
