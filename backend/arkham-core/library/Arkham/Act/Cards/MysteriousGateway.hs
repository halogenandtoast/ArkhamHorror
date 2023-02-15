module Arkham.Act.Cards.MysteriousGateway where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Types
import Arkham.Act.Helpers
import Arkham.Act.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.GameValue
import Arkham.Id
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillType
import Arkham.Source
import Arkham.Target

newtype MysteriousGateway = MysteriousGateway ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

mysteriousGateway :: ActCard MysteriousGateway
mysteriousGateway = act
  (1, A)
  MysteriousGateway
  Cards.mysteriousGateway
  (Just $ GroupClueCost (PerPlayer 3) (LocationWithTitle "Guest Hall"))

instance RunMessage MysteriousGateway where
  runMessage msg a@(MysteriousGateway attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ _ | aid == actId && onSide B attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- selectList $ InvestigatorAt $ LocationWithTitle "Guest Hall"
      holeInTheWall <- getSetAsideCard Locations.holeInTheWall
      pushAll
        [ PlaceLocation holeInTheWall
        , chooseOne
            leadInvestigatorId
            [ targetLabel iid'
               [ MoveTo (toSource attrs) iid' $ LocationId $ toCardId holeInTheWall
               , beginSkillTest
                 iid'
                 (ActSource aid)
                 (InvestigatorTarget iid')
                 Nothing
                 SkillWillpower
                 4
               ]
            | iid' <- investigatorIds
            ]
        , AdvanceActDeck actDeckId (toSource attrs)
        ]
      pure a
    FailedSkillTest iid _ (ActSource aid) SkillTestInitiatorTarget{} _ n
      | aid == actId -> do
      pushAll $ replicate n $ RandomDiscard iid (toSource attrs) AnyCard
      pure a
    _ -> MysteriousGateway <$> runMessage msg attrs
