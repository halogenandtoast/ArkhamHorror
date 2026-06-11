module Arkham.Enemy.Cards.BringerOfParadise (bringerOfParadise, bringerOfParadiseWarOfTheOuterGods) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Scenarios.WarOfTheOuterGods.Helpers

newtype BringerOfParadise = BringerOfParadise EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bringerOfParadise :: EnemyCard BringerOfParadise
bringerOfParadise =
  enemyWith BringerOfParadise Cards.bringerOfParadise (3, Static 3, 1) (1, 0)
    $ spawnAtL
    ?~ SpawnAt (locationIs Locations.shrineOfMaghanArkat)

bringerOfParadiseWarOfTheOuterGods :: EnemyCard BringerOfParadise
bringerOfParadiseWarOfTheOuterGods =
  enemyWith BringerOfParadise Cards.bringerOfParadiseWarOfTheOuterGods (3, Static 3, 1) (1, 0)
    $ spawnAtL
    ?~ SpawnAt (locationIs Locations.shrineOfMaghanArkat)

instance HasAbilities BringerOfParadise where
  getAbilities (BringerOfParadise a) =
    extend1 a $ mkAbility a 1 $ forced $ RoundEnds #when

instance RunMessage BringerOfParadise where
  runMessage msg e@(BringerOfParadise attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeMutations (attrs.ability 1) attrs.id 1
      doStep 1 msg
      pure e
    DoStep 1 (UseThisAbility _ (isSource attrs -> True) 1) -> do
      mutations <- getMutations attrs.id
      when (mutations >= 3) do
        removeMutations (attrs.ability 1) attrs.id 2
        placeDoomOnFactionAgenda (attrs.ability 1) GreenFaction 2
      pure e
    _ -> BringerOfParadise <$> liftRunMessage msg attrs
