module Arkham.Treachery.Cards.Mutated1 (mutated1) where

import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Source
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Mutated1 = Mutated1 TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mutated1 :: TreacheryCard Mutated1
mutated1 = treachery Mutated1 Cards.mutated1

instance RunMessage Mutated1 where
  runMessage msg t@(Mutated1 attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower
        $ IfEnemyExistsCalculation (enemyAtLocationWith iid) (Fixed 4) (Fixed 2)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      investigators <- select $ InvestigatorAt (locationWithInvestigator iid)
      chooseOneM iid $ withI18n do
        countVar 2 $ labeled' "takeDamage" $ assignDamage iid attrs 2
        labeledValidate' (notNull investigators) "core2.mutated.option " do
          for_ investigators \i -> assignHorror i attrs 1
      pure t
    _ -> Mutated1 <$> liftRunMessage msg attrs
