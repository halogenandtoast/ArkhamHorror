module Arkham.Enemy.Cards.ElderDeepOne (elderDeepOne) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Trait (Trait (DeepOne))

newtype ElderDeepOne = ElderDeepOne EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

elderDeepOne :: EnemyCard ElderDeepOne
elderDeepOne = enemy ElderDeepOne Cards.elderDeepOne

instance HasModifiersFor ElderDeepOne where
  getModifiersFor (ElderDeepOne a) =
    modifySelect a (EnemyWithTrait DeepOne <> not_ (be a)) [AddKeyword Keyword.Relentless]

instance HasAbilities ElderDeepOne where
  getAbilities (ElderDeepOne a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyEngaged #after You (be a)

instance RunMessage ElderDeepOne where
  runMessage msg e@(ElderDeepOne attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectEach (EnemyWithTrait DeepOne <> not_ (be attrs)) \eid -> healDamage eid (attrs.ability 1) 1
      pure e
    _ -> ElderDeepOne <$> liftRunMessage msg attrs
