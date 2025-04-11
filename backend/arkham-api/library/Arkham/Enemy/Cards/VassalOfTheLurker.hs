module Arkham.Enemy.Cards.VassalOfTheLurker (vassalOfTheLurker) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype VassalOfTheLurker = VassalOfTheLurker EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

vassalOfTheLurker :: EnemyCard VassalOfTheLurker
vassalOfTheLurker = enemy VassalOfTheLurker Cards.vassalOfTheLurker (3, Static 4, 2) (1, 1)

instance HasAbilities VassalOfTheLurker where
  getAbilities (VassalOfTheLurker a) =
    extend1 a
      $ restricted a 1 (exists $ InvestigatorAt (locationWithEnemy a))
      $ forced
      $ PhaseEnds #when #enemy

instance RunMessage VassalOfTheLurker where
  runMessage msg e@(VassalOfTheLurker attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      investigators <- select $ InvestigatorAt (locationWithEnemy attrs)
      for_ investigators \iid -> discardTopOfDeck iid (attrs.ability 1) 1
      pure e
    _ -> VassalOfTheLurker <$> liftRunMessage msg attrs
