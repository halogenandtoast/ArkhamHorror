module Arkham.Types.Act.Cards.MysteriousGateway where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import qualified Arkham.Location.Cards as Locations
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.InvestigatorId
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target

newtype MysteriousGateway = MysteriousGateway ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasActions, HasModifiersFor env)

mysteriousGateway :: ActCard MysteriousGateway
mysteriousGateway = act
  (1, A)
  MysteriousGateway
  Cards.mysteriousGateway
  (Just $ GroupClueCost (PerPlayer 3) (Just $ LocationWithTitle "Guest Hall"))

instance ActRunner env => RunMessage env MysteriousGateway where
  runMessage msg a@(MysteriousGateway attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ | aid == actId && onSide A attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getSetList @InvestigatorId
        (LocationWithTitle "Guest Hall")
      requiredClues <- getPlayerCountValue (PerPlayer 3)
      pushAll
        [ SpendClues requiredClues investigatorIds
        , chooseOne leadInvestigatorId [AdvanceAct aid (toSource attrs)]
        ]
      pure $ MysteriousGateway $ attrs & (sequenceL .~ Act 1 B)
    AdvanceAct aid _ | aid == actId && onSide B attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getSetList @InvestigatorId
        (LocationWithTitle "Guest Hall")
      holeInTheWallId <- getRandom
      a <$ pushAll
        ([PlaceLocation holeInTheWallId Locations.holeInTheWall]
        <> [ chooseOne
             leadInvestigatorId
             [ TargetLabel
                 (InvestigatorTarget iid')
                 [ MoveTo iid' holeInTheWallId
                 , MovedBy iid' (toSource attrs)
                 , BeginSkillTest
                   iid'
                   (ActSource aid)
                   (InvestigatorTarget iid')
                   Nothing
                   SkillWillpower
                   4
                 ]
             | iid' <- investigatorIds
             ]
           , NextAct aid "01109"
           ]
        )
    FailedSkillTest iid _ (ActSource aid) SkillTestInitiatorTarget{} _ n
      | aid == actId -> a <$ pushAll (replicate n (RandomDiscard iid))
    _ -> MysteriousGateway <$> runMessage msg attrs
