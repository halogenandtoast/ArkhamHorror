module Arkham.Treachery.Cards.SerpentsCall (serpentsCall, SerpentsCall (..)) where

import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype SerpentsCall = SerpentsCall TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

serpentsCall :: TreacheryCard SerpentsCall
serpentsCall = treachery SerpentsCall Cards.serpentsCall

instance RunMessage SerpentsCall where
  runMessage msg t@(SerpentsCall attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      getIsPoisoned iid >>= \case
        True -> drawEncounterCards iid attrs 2
        False -> do
          poisoned <- getSetAsidePoisoned
          chooseOneM iid do
            labeled "Put a set-aside Poisoned weakness into play in your threat area" do
              push $ CreateWeaknessInThreatArea poisoned iid
            labeled "Draw the top 2 cards of the encounter deck" do
              drawEncounterCards iid attrs 2
      pure t
    _ -> SerpentsCall <$> liftRunMessage msg attrs
