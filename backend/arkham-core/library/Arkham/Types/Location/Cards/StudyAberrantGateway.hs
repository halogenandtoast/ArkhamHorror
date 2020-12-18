{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.StudyAberrantGateway where

import Arkham.Import

import Arkham.Types.Action (Action)
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Game.Helpers
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner

newtype StudyAberrantGateway = StudyAberrantGateway Attrs
  deriving newtype (Show, ToJSON, FromJSON)

studyAberrantGateway :: StudyAberrantGateway
studyAberrantGateway = StudyAberrantGateway $ baseAttrs
  "50013"
  "Study"
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
    | iid `elem` locationInvestigators attrs = do
      baseActions <- getActions iid NonFast attrs
      leadInvestigatorId <- getLeadInvestigatorId
      canActivate <- (>= 2) . unActionRemainingCount <$> getCount
        (Nothing :: Maybe Action, setToList (locationTraits attrs), iid)
      pure
        $ baseActions
        <> [ ActivateCardAbilityAction
               iid
               (mkAbility (toSource attrs) 1 (ActionAbility 2 Nothing))
           | canActivate && leadInvestigatorId == iid
           ]
  getActions iid window (StudyAberrantGateway attrs) =
    getActions iid window attrs

instance LocationRunner env => RunMessage env StudyAberrantGateway where
  runMessage msg l@(StudyAberrantGateway attrs@Attrs {..}) = case msg of
    UseCardAbility iid (LocationSource lid) _ 1 | lid == locationId ->
      l <$ unshiftMessage (DrawCards iid 3 False)
    When (EnemySpawnAtLocationNamed _ locationName' _) -> do
      locations' <- getList @LocationName ()
      l <$ unless
        (locationName' `elem` locations')
        (unshiftMessage (PlaceLocationNamed locationName'))
    _ -> StudyAberrantGateway <$> runMessage msg attrs
