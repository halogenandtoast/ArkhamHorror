module Arkham.Treachery.Cards.Apeirophobia (apeirophobia, Apeirophobia (..)) where

import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Helpers.ChaosBag
import Arkham.Helpers.Investigator (canPlaceCluesOnYourLocation)
import Arkham.Helpers.Location (withLocationOf)
import Arkham.I18n
import Arkham.Investigator.Projection ()
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Apeirophobia = Apeirophobia TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

apeirophobia :: TreacheryCard Apeirophobia
apeirophobia = treachery Apeirophobia Cards.apeirophobia

instance RunMessage Apeirophobia where
  runMessage msg t@(Apeirophobia attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      withLocationOf iid \lid -> do
        shelterValue lid >>= traverse_ \shelter -> do
          sid <- getRandom
          revelationSkillTest sid iid attrs #willpower (Fixed shelter)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      canPlaceClues <- canPlaceCluesOnYourLocation iid
      chooseOneM iid $ withI18n do
        countVar n $ labeled' "takeHorrorForEachPointOfFailure" $ assignHorror iid attrs n
        when canPlaceClues do
          countVar 2 $ labeled' "placeCluesOnYourLocation" $ placeCluesOnLocation iid attrs 2
        whenM hasRemainingFrostTokens do
          campaignI18n $ scope "apeirophobia" $ labeled' "addFrostToken" $ addChaosToken #frost
      pure t
    _ -> Apeirophobia <$> liftRunMessage msg attrs
