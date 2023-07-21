module Arkham.Act.Cards.MysteriousGateway where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Helpers
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message
import Arkham.Movement
import Arkham.SkillType

newtype MysteriousGateway = MysteriousGateway ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

mysteriousGateway :: ActCard MysteriousGateway
mysteriousGateway =
  act
    (1, A)
    MysteriousGateway
    Cards.mysteriousGateway
    (Just $ GroupClueCost (PerPlayer 3) (LocationWithTitle "Guest Hall"))

instance RunMessage MysteriousGateway where
  runMessage msg a@(MysteriousGateway attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ _ | aid == actId && onSide B attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- selectList $ InvestigatorAt $ LocationWithTitle "Guest Hall"
      (holeInTheWallId, placeHoleInTheWall) <- placeSetAsideLocation Locations.holeInTheWall
      pushAll
        [ placeHoleInTheWall
        , chooseOne
            leadInvestigatorId
            [ targetLabel
              iid'
              [ MoveTo $ move (toSource attrs) iid' holeInTheWallId
              , beginSkillTest iid' (ActSource aid) (InvestigatorTarget iid') SkillWillpower 4
              ]
            | iid' <- investigatorIds
            ]
        , AdvanceActDeck actDeckId (toSource attrs)
        ]
      pure a
    FailedSkillTest iid _ (ActSource aid) SkillTestInitiatorTarget {} _ n
      | aid == actId -> do
          pushAll $ replicate n $ toMessage $ randomDiscard iid attrs
          pure a
    _ -> MysteriousGateway <$> runMessage msg attrs
