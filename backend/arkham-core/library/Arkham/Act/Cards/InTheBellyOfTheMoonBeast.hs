module Arkham.Act.Cards.InTheBellyOfTheMoonBeast (InTheBellyOfTheMoonBeast (..), inTheBellyOfTheMoonBeast) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.Helpers.Campaign (getOwner)
import Arkham.Helpers.Query (getLead)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher

newtype InTheBellyOfTheMoonBeast = InTheBellyOfTheMoonBeast ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

inTheBellyOfTheMoonBeast :: ActCard InTheBellyOfTheMoonBeast
inTheBellyOfTheMoonBeast =
  act
    (1, A)
    InTheBellyOfTheMoonBeast
    Cards.inTheBellyOfTheMoonBeast
    (Just $ GroupClueCost (PerPlayer 2) (locationIs Locations.cityOfTheMoonBeasts))

instance RunMessage InTheBellyOfTheMoonBeast where
  runMessage msg a@(InTheBellyOfTheMoonBeast attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      investigators <- select $ InvestigatorAt "City of the Moon-Beasts"
      lead <- getLead
      virgilGray <- getSetAsideCard Assets.virgilGrayTrulyInspired
      chooseOrRunOne
        lead
        [targetLabel iid [TakeControlOfSetAsideAsset iid virgilGray] | iid <- investigators]

      whenHasRecord RandolphWasCaptured do
        mOwner <- getOwner Assets.randolphCarterExpertDreamer
        for_ mOwner \iid -> do
          randolph <- getSetAsideCard Assets.randolphCarterExpertDreamer
          addToHand iid [randolph]

      advanceActDeck attrs
      pure a
    _ -> InTheBellyOfTheMoonBeast <$> lift (runMessage msg attrs)
