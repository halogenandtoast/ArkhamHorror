module Arkham.Enemy.Cards.GrotesqueLionCircusExMortis (grotesqueLionCircusExMortis) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher

newtype GrotesqueLionCircusExMortis = GrotesqueLionCircusExMortis EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grotesqueLionCircusExMortis :: EnemyCard GrotesqueLionCircusExMortis
grotesqueLionCircusExMortis = enemy GrotesqueLionCircusExMortis Cards.grotesqueLionCircusExMortis

instance HasModifiersFor GrotesqueLionCircusExMortis where
  getModifiersFor (GrotesqueLionCircusExMortis a) = modifySelf a [AddKeyword Keyword.Hunter]

instance HasAbilities GrotesqueLionCircusExMortis where
  getAbilities (GrotesqueLionCircusExMortis a) =
    extend1 a
      $ mkAbility a 1
      $ SilentForcedAbility
      $ EnemyEntersPlay #after (be a)

instance RunMessage GrotesqueLionCircusExMortis where
  runMessage msg e@(GrotesqueLionCircusExMortis attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      roundModifier (attrs.ability 1) attrs (EnemyFight 1)
      roundModifier (attrs.ability 1) attrs (EnemyEvade (-1))
      pure e
    _ -> GrotesqueLionCircusExMortis <$> liftRunMessage msg attrs
