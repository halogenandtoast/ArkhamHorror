module Arkham.Treachery.Cards.TerrorGate (terrorGate) where

import Arkham.Helpers.Message.Discard.Lifted
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Rival))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype TerrorGate = TerrorGate TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

terrorGate :: TreacheryCard TerrorGate
terrorGate = treachery TerrorGate Cards.terrorGate

instance RunMessage TerrorGate where
  runMessage msg t@(TerrorGate attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower
        $ SumCalculation [Fixed 2, IfEnemyExistsCalculation (EnemyWithTrait Rival) (Fixed 2) (Fixed 0)]
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      chooseOneM iid $ withI18n do
        countVar 2 $ labeled' "takeDamage" $ assignDamage iid attrs 2
        countVar 2 $ labeled' "discardCards" $ chooseAndDiscardCards iid attrs 2
        countVar 3 $ labeled' "loseResources" $ loseResources iid attrs 3
        countVar 2 $ labeled' "takeHorror" $ assignHorror iid attrs 2
        countVar 1 $ labeled' "loseActions" $ loseActions iid attrs 1
      pure t
    _ -> TerrorGate <$> liftRunMessage msg attrs
