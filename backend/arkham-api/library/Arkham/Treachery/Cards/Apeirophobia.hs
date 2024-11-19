module Arkham.Treachery.Cards.Apeirophobia (apeirophobia, Apeirophobia (..)) where

import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Helpers.ChaosBag
import Arkham.Helpers.Investigator (withLocationOf)
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
      clues <- iid.clues
      chooseOneM iid do
        labeled "Take 1 horror for each point you failed by." $ assignHorror iid attrs n
        when (clues >= 2) do
          labeled "Place 2 of your clues on your location." $ placeCluesOnLocation iid attrs 2
        whenM hasRemainingFrostTokens do
          labeled "Add 1 {frost} token to the chaos bag." $ addChaosToken #frost
      pure t
    _ -> Apeirophobia <$> liftRunMessage msg attrs
