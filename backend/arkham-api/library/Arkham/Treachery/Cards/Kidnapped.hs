module Arkham.Treachery.Cards.Kidnapped (kidnapped) where

import Arkham.Ability
import Arkham.Helpers.Choose
import Arkham.I18n
import Arkham.Matcher hiding (PlaceUnderneath)
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Deck
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Kidnapped = Kidnapped TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

kidnapped :: TreacheryCard Kidnapped
kidnapped = treachery Kidnapped Cards.kidnapped

instance HasAbilities Kidnapped where
  getAbilities (Kidnapped attrs) = case attrs.attached of
    Just (AgendaTarget aid) -> [mkAbility attrs 1 $ forced $ AgendaAdvances #when $ AgendaWithId aid]
    _ -> []

instance RunMessage Kidnapped where
  runMessage msg t@(Kidnapped attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      chooseOneM iid $ withI18n do
        chooseTest #willpower 4 $ revelationSkillTest sid iid attrs #willpower (Fixed 4)
        chooseTest #agility 4 $ revelationSkillTest sid iid attrs #agility (Fixed 4)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      allies <- select $ assetControlledBy iid <> #ally
      if null allies
        then assignDamage iid attrs 2
        else do
          chooseTargetM iid allies $ push . AddToScenarioDeck PotentialSacrifices . toTarget
          selectJust AnyAgenda >>= attachTreachery attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      randomlyChooseFrom attrs iid PotentialSacrifices 1
      pure t
    ChoseCards _ chosen | isTarget attrs chosen.target -> do
      placeUnderneath AgendaDeckTarget chosen.cards
      pure t
    _ -> Kidnapped <$> liftRunMessage msg attrs
