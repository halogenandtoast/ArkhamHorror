{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Act.Cards.MysteriousGateway where

import Arkham.Import

import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner
import Arkham.Types.LocationMatcher

newtype MysteriousGateway = MysteriousGateway Attrs
  deriving newtype (Show, ToJSON, FromJSON)

mysteriousGateway :: MysteriousGateway
mysteriousGateway = MysteriousGateway $ baseAttrs
  "50012"
  "Mysterious Gateway"
  (Act 1 A)
  (Just $ RequiredClues (PerPlayer 3) (Just $ LocationNamed "Guest Hall"))

instance ActionRunner env => HasActions env MysteriousGateway where
  getActions i window (MysteriousGateway x) = getActions i window x

instance ActRunner env => RunMessage env MysteriousGateway where
  runMessage msg a@(MysteriousGateway attrs@Attrs {..}) = case msg of
    AdvanceAct aid | aid == actId && actSequence == Act 1 A -> do
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getSetList @InvestigatorId (LocationId "50014")
      requiredClues <- getPlayerCountValue (PerPlayer 3)
      unshiftMessages
        [ SpendClues requiredClues investigatorIds
        , chooseOne leadInvestigatorId [AdvanceAct aid]
        ]
      pure
        $ MysteriousGateway
        $ attrs
        & (sequenceL .~ Act 1 B)
        & (flippedL .~ True)
    AdvanceAct aid | aid == actId && actSequence == Act 1 B -> do
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getSetList @InvestigatorId (LocationId "50014")
      a <$ unshiftMessages
        ([PlaceLocation "50017"]
        <> [ chooseOne
             leadInvestigatorId
             [ TargetLabel
                 (InvestigatorTarget iid')
                 [ MoveTo iid' "50017"
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
    FailedSkillTest iid _ (ActSource aid) SkillTestInitiatorTarget{} n
      | aid == actId -> a <$ unshiftMessages (replicate n (RandomDiscard iid))
    _ -> MysteriousGateway <$> runMessage msg attrs
