module Arkham.Enemy.Cards.Mother (mother) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Enemy.Types (Field (EnemyDoom))
import Arkham.Helpers.Window (defeatedEnemy)
import Arkham.Matcher
import Arkham.Projection
import Arkham.Trait (Trait (..))

newtype Mother = Mother EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mother :: EnemyCard Mother
mother = enemy Mother Cards.mother (4, Static 8, 1) (1, 1)

instance HasAbilities Mother where
  getAbilities (Mother a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ EnemyDefeated #when Anyone ByAny (not_ (be a) <> EnemyWithTrait Stowaway)

instance RunMessage Mother where
  runMessage msg e@(Mother attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (defeatedEnemy -> eid) _ -> do
      moveAllTokens (attrs.ability 1) eid attrs #damage
      hasDoom <- fieldP EnemyDoom (> 0) eid
      when hasDoom $ shuffleBackIntoEncounterDeck eid
      pure e
    _ -> Mother <$> liftRunMessage msg attrs
