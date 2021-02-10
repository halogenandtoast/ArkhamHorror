module Arkham.Types.Enemy.Cards.TheMaskedHunter where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.GameValue
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Prey
import Arkham.Types.Query
import Arkham.Types.Target
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Enemy.Runner

newtype TheMaskedHunter = TheMaskedHunter EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theMaskedHunter :: EnemyId -> TheMaskedHunter
theMaskedHunter uuid =
  TheMaskedHunter
    $ baseAttrs uuid "01121b"
    $ (healthDamageL .~ 2)
    . (sanityDamageL .~ 1)
    . (fightL .~ 4)
    . (healthL .~ Static 4)
    . (evadeL .~ 2)
    . (preyL .~ MostClues)
    . (uniqueL .~ True)

instance HasModifiersFor env TheMaskedHunter where
  getModifiersFor _ (InvestigatorTarget iid) (TheMaskedHunter a@EnemyAttrs {..}) =
    if iid `elem` enemyEngagedInvestigators
      then pure $ toModifiers a [CannotDiscoverClues, CannotSpendClues]
      else pure []
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env TheMaskedHunter where
  getActions i window (TheMaskedHunter attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env TheMaskedHunter where
  runMessage msg (TheMaskedHunter attrs@EnemyAttrs {..}) = case msg of
    EnemySpawnEngagedWithPrey eid | eid == enemyId -> do
      playerCount <- unPlayerCount <$> getCount ()
      TheMaskedHunter
        <$> runMessage msg (attrs & healthL %~ fmap (+ (2 * playerCount)))
    _ -> TheMaskedHunter <$> runMessage msg attrs
