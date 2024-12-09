module Arkham.Enemy.Cards.TheTerrorOfDevilReef_165 (
  theTerrorOfDevilReef_165,
  TheTerrorOfDevilReef_165 (..),
)
where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyDefeated)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Matcher
import Arkham.Trait (Trait (Cave))

newtype TheTerrorOfDevilReef_165 = TheTerrorOfDevilReef_165 EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTerrorOfDevilReef_165 :: EnemyCard TheTerrorOfDevilReef_165
theTerrorOfDevilReef_165 = enemy TheTerrorOfDevilReef_165 Cards.theTerrorOfDevilReef_165 (3, Static 6, 3) (2, 2)

instance HasAbilities TheTerrorOfDevilReef_165 where
  getAbilities (TheTerrorOfDevilReef_165 a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyDefeated #when Anyone ByAny (be a)

instance HasModifiersFor TheTerrorOfDevilReef_165 where
  getModifiersFor (TheTerrorOfDevilReef_165 a) =
    modifySelect a (LocationWithTrait Cave) [CannotBeEnteredBy (be a)]

instance RunMessage TheTerrorOfDevilReef_165 where
  runMessage msg e@(TheTerrorOfDevilReef_165 attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      insteadOfDefeat attrs do
        healAllDamage (attrs.ability 1) attrs
        exhaustThis attrs
        roundModifier (attrs.ability 1) attrs CannotReady
      pure e
    _ -> TheTerrorOfDevilReef_165 <$> liftRunMessage msg attrs
