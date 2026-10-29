module Arkham.Homebrew.AgesUnwound.Enemies.AgelessWatchers (agelessWatchers) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted hiding (PlayCard)
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Cards
import Arkham.Matcher
import Arkham.Window (windowType)
import Arkham.Window qualified as Window

newtype AgelessWatchers = AgelessWatchers EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

agelessWatchers :: EnemyCard AgelessWatchers
agelessWatchers =
  enemyWith AgelessWatchers Cards.agelessWatchers
    $ (spawnAtL ?~ SpawnAt (FarthestLocationFromAll EmptyLocation))
    . (surgeIfUnableToSpawnL .~ True)

instance HasAbilities AgelessWatchers where
  getAbilities (AgelessWatchers a) =
    extend1 a $ restricted a 1 (thisIs a IsHost) $ forced $ PlayCard #after Anyone (basic #event)

instance RunMessage AgelessWatchers where
  runMessage msg e@(AgelessWatchers attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws _ -> do
      for_ [cp.card | (windowType -> Window.PlayCard _ cp) <- ws] \card -> do
        obtainCard card
        push $ PlacedSwarmCard attrs.id card
      pure e
    _ -> AgelessWatchers <$> liftRunMessage msg attrs
