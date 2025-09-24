module Arkham.Treachery.Cards.FateOfAllFools (fateOfAllFools) where

import Arkham.Campaigns.TheCircleUndone.Helpers
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype FateOfAllFools = FateOfAllFools TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fateOfAllFools :: TreacheryCard FateOfAllFools
fateOfAllFools = treachery FateOfAllFools Cards.fateOfAllFools

instance RunMessage FateOfAllFools where
  runMessage msg t@(FateOfAllFools attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      select (HasMatchingTreachery $ TreacheryWithTitle "Fate of All Fools") >>= \case
        [] -> placeInThreatArea attrs iid
        iids -> do
          fates <- select $ TreacheryWithTitle "Fate of All Fools" <> TreacheryInThreatAreaOf Anyone
          chooseOneM iid $ campaignI18n do
            labeled' "fateOfAllFools.otherInvestigator" do
              chooseTargetM iid iids \iid' -> directDamage iid' attrs 2
            labeled' "fateOfAllFools.otherCopy" do
              chooseTargetM iid fates $ placeDoomOn attrs 1
      pure t
    _ -> FateOfAllFools <$> liftRunMessage msg attrs
