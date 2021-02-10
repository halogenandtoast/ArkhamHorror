module Arkham.Types.Act.Cards.FindingAWayInside
  ( FindingAWayInside(..)
  , findingAWayInside
  ) where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.ActId
import Arkham.Types.AgendaId
import Arkham.Types.AssetId
import Arkham.Types.CampaignId
import Arkham.Types.Card
import Arkham.Types.Card.Cost
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.ClassSymbol
import Arkham.Types.Cost
import Arkham.Types.Direction
import Arkham.Types.Effect.Window
import Arkham.Types.EffectId
import Arkham.Types.EffectMetadata
import Arkham.Types.EncounterSet (EncounterSet)
import Arkham.Types.EnemyId
import Arkham.Types.EventId
import Arkham.Types.Exception
import Arkham.Types.GameValue
import Arkham.Types.Helpers
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.Prey
import Arkham.Types.Query
import Arkham.Types.Resolution
import Arkham.Types.ScenarioId
import Arkham.Types.SkillId
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Stats (Stats)
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.TreacheryId
import Arkham.Types.Window


import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner

newtype FindingAWayInside = FindingAWayInside ActAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

findingAWayInside :: FindingAWayInside
findingAWayInside = FindingAWayInside $ baseAttrs
  "02122"
  "Finding A Way Inside"
  (Act 1 A)
  (Just $ RequiredClues (Static 2) Nothing)

instance ActionRunner env => HasActions env FindingAWayInside where
  getActions i window (FindingAWayInside x) = getActions i window x

instance ActRunner env => RunMessage env FindingAWayInside where
  runMessage msg a@(FindingAWayInside attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ | aid == actId && onSide A attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getInvestigatorIds
      unshiftMessages
        [ SpendClues 2 investigatorIds
        , chooseOne leadInvestigatorId [AdvanceAct aid (toSource attrs)]
        ]
      pure $ FindingAWayInside $ attrs & sequenceL .~ Act 1 B
    AdvanceAct aid source
      | aid == actId && onSide B attrs && isSource attrs source -> do
        leadInvestigatorId <- getLeadInvestigatorId
        investigatorIds <- getInvestigatorIds
        adamLynch <- PlayerCard <$> genPlayerCard "02139"
        a <$ unshiftMessages
          [ chooseOne
            leadInvestigatorId
            [ TargetLabel
                (InvestigatorTarget iid)
                [TakeControlOfSetAsideAsset iid adamLynch]
            | iid <- investigatorIds
            ]
          , RevealLocation Nothing "02127"
          , NextAct aid "02123"
          ]
    AdvanceAct aid _ | aid == actId && onSide B attrs ->
      a <$ unshiftMessages [RevealLocation Nothing "02127", NextAct aid "02124"]
    _ -> FindingAWayInside <$> runMessage msg attrs
