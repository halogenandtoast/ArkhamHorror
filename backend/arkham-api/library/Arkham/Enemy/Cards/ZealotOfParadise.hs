module Arkham.Enemy.Cards.ZealotOfParadise (zealotOfParadise) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Scenarios.WarOfTheOuterGods.Helpers

newtype ZealotOfParadise = ZealotOfParadise EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

zealotOfParadise :: EnemyCard ZealotOfParadise
zealotOfParadise =
  enemy ZealotOfParadise Cards.zealotOfParadise
    & setSpawnAt (locationIs Locations.shrineOfMaghanArkat)

instance HasAbilities ZealotOfParadise where
  getAbilities (ZealotOfParadise a) =
    extend1 a
      $ restricted a 1 (noneInPlay $ mapOneOf factionEnemy [BlueFaction, RedFaction])
      $ forced
      $ RoundEnds #when

instance RunMessage ZealotOfParadise where
  runMessage msg e@(ZealotOfParadise attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeDoomOnFactionAgenda (attrs.ability 1) GreenFaction 1
      pure e
    _ -> ZealotOfParadise <$> liftRunMessage msg attrs
