module Arkham.Act.Cards.RitualOfLifeAndDeath (ritualOfLifeAndDeath) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Log
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.ScenarioLogKey

newtype RitualOfLifeAndDeath = RitualOfLifeAndDeath ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ritualOfLifeAndDeath :: ActCard RitualOfLifeAndDeath
ritualOfLifeAndDeath = act (2, A) RitualOfLifeAndDeath Cards.ritualOfLifeAndDeath Nothing

instance HasAbilities RitualOfLifeAndDeath where
  getAbilities = actAbilities \a ->
    [ restricted a 1 (HasScenarioCount CiviliansSlain (AtLeast $ PerPlayer 4))
        $ forced
        $ ScenarioCountIncremented #after CiviliansSlain
    , mkAbility a 2
        $ Objective
        $ forced
        $ EnemyWouldBeDefeated #when (enemyIs Enemies.amaranthLurkingCorruption)
    ]

instance RunMessage RitualOfLifeAndDeath where
  runMessage msg a@(RitualOfLifeAndDeath attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      advanceVia #other attrs (attrs.ability 1)
      pure a
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      advanceVia #other attrs (attrs.ability 1)
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      n <- perPlayer 4
      wasForced <- (>= n) <$> scenarioCount CiviliansSlain
      if wasForced
        then do
          razin <- fetchCard Enemies.razinFarhiReanimatedArtificer
          bahiaPalaceGardens <- selectJust $ location_ "Bahia Palace Gardens"
          createEnemy_ razin bahiaPalaceGardens
        else do
          lead <- getLead
          amaranth <- selectJust $ enemyIs Enemies.amaranthLurkingCorruption
          cancelEnemyDefeat amaranth
          flipOver lead amaranth
          locations <- select $ not_ $ LocationWithInvestigator Anyone
          case nonEmpty locations of
            Nothing -> error "impossible"
            Just xs -> sample xs >>= enemyMoveTo attrs amaranth
      shuffleSetAsideIntoEncounterDeck $ cardIs Enemies.ancientRaider
      advanceActDeck attrs
      pure a
    _ -> RitualOfLifeAndDeath <$> liftRunMessage msg attrs
