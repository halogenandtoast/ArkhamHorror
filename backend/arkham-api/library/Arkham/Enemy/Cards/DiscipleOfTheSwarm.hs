module Arkham.Enemy.Cards.DiscipleOfTheSwarm (discipleOfTheSwarm) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Scenarios.WarOfTheOuterGods.Helpers

newtype DiscipleOfTheSwarm = DiscipleOfTheSwarm EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

discipleOfTheSwarm :: EnemyCard DiscipleOfTheSwarm
discipleOfTheSwarm =
  enemy DiscipleOfTheSwarm Cards.discipleOfTheSwarm
    & setSpawnAt (locationIs Locations.theBurningPit)

instance HasAbilities DiscipleOfTheSwarm where
  getAbilities (DiscipleOfTheSwarm a) =
    extend1 a
      $ restricted a 1 (noneInPlay $ mapOneOf factionEnemy [BlueFaction, GreenFaction])
      $ forced
      $ RoundEnds #when

instance RunMessage DiscipleOfTheSwarm where
  runMessage msg e@(DiscipleOfTheSwarm attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeDoomOnFactionAgenda (attrs.ability 1) RedFaction 1
      pure e
    _ -> DiscipleOfTheSwarm <$> liftRunMessage msg attrs
