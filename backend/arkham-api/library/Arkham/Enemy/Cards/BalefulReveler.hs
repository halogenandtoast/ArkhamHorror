module Arkham.Enemy.Cards.BalefulReveler (balefulReveler) where

import Arkham.Ability
import Arkham.ChaosToken
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.ChaosToken
import Arkham.Helpers.Investigator
import Arkham.Helpers.Query
import Arkham.Matcher
import Arkham.Scenarios.CarnevaleOfHorrors.Helpers
import Control.Monad.Extra (findM)

newtype BalefulReveler = BalefulReveler EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- TODO: spawn at is complicated here
balefulReveler :: EnemyCard BalefulReveler
balefulReveler = enemy BalefulReveler Cards.balefulReveler (4, PerPlayer 5, 3) (2, 2)

instance HasAbilities BalefulReveler where
  getAbilities (BalefulReveler a) =
    extend1 a $ groupLimit PerRound $ mkAbility a 1 $ forced $ MovedFromHunter #after (be a)

instance RunMessage BalefulReveler where
  runMessage msg e@(BalefulReveler attrs) = runQueueT $ case msg of
    InvestigatorDrawEnemy _ eid | eid == toId attrs -> do
      leadInvestigatorId <- getLead
      start <- getJustLocation leadInvestigatorId
      locations <- getCounterClockwiseLocations start
      spawnLocation <-
        maybe Nowhere LocationWithId
          <$> findM (selectNone . InvestigatorAt . LocationWithId) locations

      BalefulReveler <$> liftRunMessage msg (attrs & spawnAtL ?~ SpawnAt spawnLocation)
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      requestChaosTokens iid (attrs.ability 1) 1
      pure e
    RequestedChaosTokens (isAbilitySource attrs 1 -> True) (Just iid) tokens -> do
      resetChaosTokens (attrs.ability 1)
      chaosTokenFaces <- getModifiedChaosTokenFaces tokens
      continue_ iid
      when (any (`elem` [Skull, Cultist, Tablet, ElderThing, AutoFail]) chaosTokenFaces) do
        push $ HunterMove attrs.id
      pure e
    _ -> BalefulReveler <$> liftRunMessage msg attrs
