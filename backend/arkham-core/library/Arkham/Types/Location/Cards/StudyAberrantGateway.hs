module Arkham.Types.Location.Cards.StudyAberrantGateway
  ( StudyAberrantGateway(..)
  , studyAberrantGateway
  )
where


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
