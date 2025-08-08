module Arkham.Act.Cards.CavernOfTheForgottenAge (cavernOfTheForgottenAge) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Helpers.Investigator
import Arkham.Helpers.Query
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Scenario.Deck

newtype CavernOfTheForgottenAge = CavernOfTheForgottenAge ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

cavernOfTheForgottenAge :: ActCard CavernOfTheForgottenAge
cavernOfTheForgottenAge =
  act
    (1, A)
    CavernOfTheForgottenAge
    Cards.cavernOfTheForgottenAge
    (Just $ GroupClueCost (PerPlayer 3) Anywhere)

instance RunMessage CavernOfTheForgottenAge where
  runMessage msg a@(CavernOfTheForgottenAge attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      descentToYoth <- getSetAsideCard Locations.descentToYoth
      shuffleCardsIntoDeck ExplorationDeck [descentToYoth]
      lead <- getLead
      iids <- getInvestigators
      chooseOrRunOneM lead $ targets iids $ handleTarget lead attrs
      advanceActDeck attrs
      pure a
    HandleTargetChoice lead (isSource attrs -> True) (InvestigatorTarget iid) -> do
      withChalk <- getHasSupply iid Chalk
      unless withChalk do
        lid <- getJustLocation iid
        investigators <- select $ investigatorAt lid
        enemies <- select $ enemyAt lid
        singleSided <-
          matches lid
            $ SingleSidedLocation
            <> NotLocation (locationIs Locations.mouthOfKnYanTheDepthsBeneath)
        mouthOfKnYan <- selectJust $ locationIs Locations.mouthOfKnYanTheDepthsBeneath
        isConnectedToMouthOfKnYan <- mouthOfKnYan <=~> ConnectedTo (LocationWithId lid)
        xs <-
          if isConnectedToMouthOfKnYan
            then pure [mouthOfKnYan]
            else
              select
                $ NearestLocationToLocation mouthOfKnYan
                $ ConnectedTo
                $ LocationWithId lid
        selectEach Anywhere \l -> placeClues attrs l 1
        when singleSided do
          chooseOneM lead do
            targets xs \l -> do
              for_ investigators \i -> moveTo attrs i l
              for_ enemies \e -> enemyMoveTo attrs e l
          shuffleIntoDeck ExplorationDeck lid
      pure a
    _ -> CavernOfTheForgottenAge <$> liftRunMessage msg attrs
