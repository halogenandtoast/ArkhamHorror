module Arkham.Enemy.Cards.GuardianElderThing (guardianElderThing) where

import Arkham.Ability
import Arkham.Card
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Window
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype GuardianElderThing = GuardianElderThing EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

guardianElderThing :: EnemyCard GuardianElderThing
guardianElderThing = enemy GuardianElderThing Cards.guardianElderThing (3, Static 4, 1) (1, 1)

instance HasAbilities GuardianElderThing where
  getAbilities (GuardianElderThing a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ EnemyDealtDamage #when AnyDamageEffect (be a) (SourceOwnedBy You)

instance RunMessage GuardianElderThing where
  runMessage msg e@(GuardianElderThing attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (dealtDamage -> n) _ -> do
      discardTopOfDeckAndHandle iid (attrs.ability 1) n attrs
      pure e
    DiscardedTopOfDeck iid cards (isAbilitySource attrs 1 -> True) (isTarget attrs -> True) -> do
      let weaknesses = filterCards WeaknessCard cards
      focusCards_ weaknesses $ chooseOneAtATimeM iid $ targets weaknesses $ drawCard iid
      pure e
    _ -> GuardianElderThing <$> liftRunMessage msg attrs
