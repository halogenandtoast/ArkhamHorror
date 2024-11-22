module Arkham.Enemy.Cards.Basilisk (basilisk, Basilisk (..)) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher

newtype Basilisk = Basilisk EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

basilisk :: EnemyCard Basilisk
basilisk =
  enemyWith Basilisk Cards.basilisk (4, Static 4, 4) (2, 0)
    $ spawnAtL
    ?~ SpawnAt (LocationWithDistanceFrom 1 (locationIs Locations.mouthOfKnYanTheCavernsMaw) Anywhere)

instance HasAbilities Basilisk where
  getAbilities (Basilisk a) =
    extend1 a
      $ limitedAbility (MaxPer Cards.basilisk PerRound 1)
      $ mkAbility a 1
      $ forced
      $ PlacedCounterOnLocation #after "Mouth of K'n-yan" AnySource #resource AnyValue

instance RunMessage Basilisk where
  runMessage msg e@(Basilisk attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ ShuffleBackIntoEncounterDeck (toTarget attrs)
      pure e
    _ -> Basilisk <$> liftRunMessage msg attrs
