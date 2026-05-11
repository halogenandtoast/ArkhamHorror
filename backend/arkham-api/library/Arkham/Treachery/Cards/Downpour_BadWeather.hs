{- HLINT ignore "Use camelCase" -}
module Arkham.Treachery.Cards.Downpour_BadWeather (downpour_BadWeather) where

import Arkham.Helpers.Investigator (canPlaceCluesOnYourLocation)
import Arkham.I18n
import Arkham.Investigator.Types (Field (..))
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Downpour_BadWeather = Downpour_BadWeather TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

downpour_BadWeather :: TreacheryCard Downpour_BadWeather
downpour_BadWeather = treachery Downpour_BadWeather Cards.downpour_BadWeather

instance RunMessage Downpour_BadWeather where
  runMessage msg t@(Downpour_BadWeather attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility (Fixed 3)
      pure t
    FailedThisSkillTestBy _iid (isSource attrs -> True) n -> do
      doStep n msg
      pure t
    DoStep n msg'@(FailedThisSkillTestBy iid (isSource attrs -> True) _) -> do
      actions <- field InvestigatorRemainingActions iid
      canPlaceClues <- canPlaceCluesOnYourLocation iid
      chooseOrRunOneM iid $ withI18n do
        countVar 1 $ labeledValidate' (actions > 0) "loseActions" do
          loseActions iid attrs 1
          doStep (n - 1) msg'
        countVar 1 $ labeledValidate' canPlaceClues "placeCluesOnYourLocation" do
          placeCluesOnLocation iid attrs 1
          doStep (n - 1) msg'
      pure t
    _ -> Downpour_BadWeather <$> liftRunMessage msg attrs
