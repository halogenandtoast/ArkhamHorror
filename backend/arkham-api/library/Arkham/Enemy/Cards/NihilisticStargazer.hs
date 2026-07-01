module Arkham.Enemy.Cards.NihilisticStargazer (nihilisticStargazer) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Scenarios.WarOfTheOuterGods.Helpers

newtype NihilisticStargazer = NihilisticStargazer EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nihilisticStargazer :: EnemyCard NihilisticStargazer
nihilisticStargazer =
  enemy NihilisticStargazer Cards.nihilisticStargazer
    & setSpawnAt (locationIs Locations.athenaeumOfTheEmptySky)

instance HasAbilities NihilisticStargazer where
  getAbilities (NihilisticStargazer a) =
    extend1 a
      $ restricted a 1 (noneInPlay $ mapOneOf factionEnemy [GreenFaction, RedFaction])
      $ forced
      $ RoundEnds #when

instance RunMessage NihilisticStargazer where
  runMessage msg e@(NihilisticStargazer attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeDoomOnFactionAgenda (attrs.ability 1) BlueFaction 1
      pure e
    _ -> NihilisticStargazer <$> liftRunMessage msg attrs
