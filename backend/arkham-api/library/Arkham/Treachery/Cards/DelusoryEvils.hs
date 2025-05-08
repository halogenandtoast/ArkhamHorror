module Arkham.Treachery.Cards.DelusoryEvils (delusoryEvils) where

import Arkham.Ability
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype DelusoryEvils = DelusoryEvils TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

delusoryEvils :: TreacheryCard DelusoryEvils
delusoryEvils = treachery DelusoryEvils Cards.delusoryEvils

instance HasAbilities DelusoryEvils where
  getAbilities (DelusoryEvils a) =
    [ restricted a 1 InYourHand
        $ forced
        $ WouldHaveSkillTestResult #when You AnySkillTest (SuccessResult (atLeast 3))
    ]

instance RunMessage DelusoryEvils where
  runMessage msg t@(DelusoryEvils attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      chooseOneM iid $ withI18n do
        nameVar attrs $ labeled' "secretlyAddToHand" $ addHiddenToHand iid attrs
        countVar 1 $ labeled' "placeAgendaDoomCanAdvance" $ placeDoomOnAgendaAndCheckAdvance 1
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      failSkillTest
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> DelusoryEvils <$> liftRunMessage msg attrs
