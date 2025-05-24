module Arkham.Enemy.Cards.TemporalDevourer (temporalDevourer) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Location
import Arkham.Matcher
import Arkham.Trait (Trait (Extradimensional, Shattered))

newtype TemporalDevourer = TemporalDevourer EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

temporalDevourer :: EnemyCard TemporalDevourer
temporalDevourer =
  enemyWith
    TemporalDevourer
    Cards.temporalDevourer
    (4, Static 5, 4)
    (1, 1)
    $ (spawnAtL ?~ SpawnAt (FarthestLocationFromYou $ hasAnyTrait [Shattered, Extradimensional]))
    . (surgeIfUnableToSpawnL .~ True)

instance HasAbilities TemporalDevourer where
  getAbilities (TemporalDevourer a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ EnemyEnters #after (hasAnyTrait [Shattered, Extradimensional]) (be a)

instance RunMessage TemporalDevourer where
  runMessage msg e@(TemporalDevourer attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      withLocationOf attrs \lid -> placeTokens (attrs.ability 1) lid #clue 1
      pure e
    _ -> TemporalDevourer <$> liftRunMessage msg attrs
