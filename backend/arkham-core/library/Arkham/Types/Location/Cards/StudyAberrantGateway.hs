module Arkham.Types.Location.Cards.StudyAberrantGateway
  ( StudyAberrantGateway(..)
  , studyAberrantGateway
  )
where

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


import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Game.Helpers
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner

newtype StudyAberrantGateway = StudyAberrantGateway LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

studyAberrantGateway :: StudyAberrantGateway
studyAberrantGateway = StudyAberrantGateway $ baseAttrs
  "50013"
  (Name "Study" (Just "Aberrant Gateway"))
  EncounterSet.ReturnToTheGathering
  3
  (PerPlayer 1)
  Circle
  [T]
  mempty

instance HasModifiersFor env StudyAberrantGateway where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env StudyAberrantGateway where
  getActions iid NonFast (StudyAberrantGateway attrs)
    | iid `elem` locationInvestigators attrs
    = withBaseActions iid NonFast attrs $ do
      leadInvestigatorId <- getLeadInvestigatorId
      pure
        [ ActivateCardAbilityAction
            iid
            (mkAbility (toSource attrs) 1 (ActionAbility Nothing $ ActionCost 2)
            )
        | leadInvestigatorId == iid
        ]
  getActions iid window (StudyAberrantGateway attrs) =
    getActions iid window attrs

instance LocationRunner env => RunMessage env StudyAberrantGateway where
  runMessage msg l@(StudyAberrantGateway attrs@LocationAttrs {..}) = case msg of
    UseCardAbility iid (LocationSource lid) _ 1 _ | lid == locationId ->
      l <$ unshiftMessage (DrawCards iid 3 False)
    When (EnemySpawnAtLocationMatching _ locationMatcher _) -> do
      inPlay <- isJust <$> getId @(Maybe LocationId) locationMatcher
      l <$ unless
        inPlay
        (unshiftMessage (PlaceLocationMatching locationMatcher))
    _ -> StudyAberrantGateway <$> runMessage msg attrs
