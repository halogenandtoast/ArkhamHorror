module Arkham.Act.Cards.InTheBellyOfTheMoonBeast (InTheBellyOfTheMoonBeast (..), inTheBellyOfTheMoonBeast) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner hiding (advanceActDeck)
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.Classes
import Arkham.Helpers.Campaign (getOwner)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted
import Arkham.Prelude

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
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      investigators <- select $ InvestigatorAt "City of the Moon-Beasts"
      lead <- getLeadPlayer
      virgilGray <- getSetAsideCard Assets.virgilGrayTrulyInspired
      push
        $ chooseOrRunOne
          lead
          [targetLabel iid [TakeControlOfSetAsideAsset iid virgilGray] | iid <- investigators]

      whenHasRecord RandolphWasCaptured do
        mOwner <- getOwner Assets.randolphCarterExpertDreamer
        for_ mOwner \iid -> do
          randolph <- getSetAsideCard Assets.randolphCarterExpertDreamer
          push $ AddToHand iid [randolph]

      advanceActDeck attrs
      pure a
    _ -> InTheBellyOfTheMoonBeast <$> lift (runMessage msg attrs)
