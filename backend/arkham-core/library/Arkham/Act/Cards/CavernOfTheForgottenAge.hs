module Arkham.Act.Cards.CavernOfTheForgottenAge (
  CavernOfTheForgottenAge (..),
  cavernOfTheForgottenAge,
) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Classes
import Arkham.Deck
import Arkham.Helpers.Investigator
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Movement
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
  runMessage msg a@(CavernOfTheForgottenAge attrs) = case msg of
    AdvanceAct aid _ _ | aid == actId attrs && onSide B attrs -> do
      descentToYoth <- getSetAsideCard Locations.descentToYoth
      iids <- getInvestigatorIds
      (leadInvestigatorId, lead) <- getLeadInvestigatorPlayer

      pushAll
        [ ShuffleCardsIntoDeck (ScenarioDeckByKey ExplorationDeck) [descentToYoth]
        , chooseOrRunOne
            lead
            [ targetLabel
              iid
              [HandleTargetChoice leadInvestigatorId (toSource attrs) (InvestigatorTarget iid)]
            | iid <- iids
            ]
        , AdvanceActDeck (actDeckId attrs) (toSource attrs)
        ]
      pure a
    HandleTargetChoice leadInvestigator (isSource attrs -> True) (InvestigatorTarget iid) -> do
      player <- getPlayer leadInvestigator
      withChalk <- getHasSupply iid Chalk
      unless withChalk $ do
        lid <- getJustLocation iid
        investigators <- selectList $ investigatorAt lid
        enemies <- selectList $ enemyAt lid
        singleSided <-
          lid
            <=~> ( SingleSidedLocation
                    <> NotLocation
                      (locationIs Locations.mouthOfKnYanTheDepthsBelow)
                 )
        mouthOfKnYan <-
          selectJust
            $ locationIs Locations.mouthOfKnYanTheDepthsBelow
        isConnectedToMouthOfKnYan <-
          mouthOfKnYan
            <=~> ConnectedTo (LocationWithId lid)
        moveTo <-
          if isConnectedToMouthOfKnYan
            then pure [mouthOfKnYan]
            else
              selectList
                $ NearestLocationToLocation mouthOfKnYan
                $ ConnectedTo
                $ LocationWithId lid
        locations <- selectList Anywhere
        pushAll
          $ [PlaceClues (toSource attrs) (toTarget l) 1 | l <- locations]
          <> ( guard singleSided
                *> [ chooseOne
                      player
                      [ targetLabel l
                        $ [MoveTo $ move attrs i l | i <- investigators]
                        <> [EnemyMove eid lid | eid <- enemies]
                      | l <- moveTo
                      ]
                   , ShuffleIntoDeck
                      (ScenarioDeckByKey ExplorationDeck)
                      (toTarget lid)
                   ]
             )
      pure a
    _ -> CavernOfTheForgottenAge <$> runMessage msg attrs
