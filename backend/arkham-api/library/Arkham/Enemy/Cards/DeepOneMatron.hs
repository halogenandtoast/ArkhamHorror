module Arkham.Enemy.Cards.DeepOneMatron (deepOneMatron) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Matcher
import Arkham.Trait (Trait (DeepOne))

newtype DeepOneMatron = DeepOneMatron EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deepOneMatron :: EnemyCard DeepOneMatron
deepOneMatron =
  enemyWith DeepOneMatron Cards.deepOneMatron
    $ spawnAtL
    ?~ SpawnAtFirst
      [ SpawnAt $ RevealedLocation <> "Drowned Shanty"
      , SpawnAt RevealedLocation
      ]

instance HasAbilities DeepOneMatron where
  getAbilities (DeepOneMatron a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyEntersPlay #after (be a)

instance RunMessage DeepOneMatron where
  runMessage msg e@(DeepOneMatron attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      findEncounterCardIn
        iid
        attrs
        (#enemy <> CardWithTrait DeepOne)
        [FromEncounterDeck, FromEncounterDiscard]
      pure e
    FoundEncounterCard _ (isTarget attrs -> True) ec -> do
      withLocationOf attrs \lid -> spawnEnemyAt_ ec lid
      pure e
    _ -> DeepOneMatron <$> liftRunMessage msg attrs
