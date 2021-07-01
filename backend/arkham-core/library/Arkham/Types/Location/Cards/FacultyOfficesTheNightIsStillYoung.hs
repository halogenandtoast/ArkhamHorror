module Arkham.Types.Location.Cards.FacultyOfficesTheNightIsStillYoung
  ( facultyOfficesTheNightIsStillYoung
  , FacultyOfficesTheNightIsStillYoung(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
  (facultyOfficesTheNightIsStillYoung)
import Arkham.Types.Ability
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Resolution
import Arkham.Types.Trait
import Arkham.Types.Window

newtype FacultyOfficesTheNightIsStillYoung = FacultyOfficesTheNightIsStillYoung LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

facultyOfficesTheNightIsStillYoung
  :: LocationId -> FacultyOfficesTheNightIsStillYoung
facultyOfficesTheNightIsStillYoung =
  FacultyOfficesTheNightIsStillYoung . baseAttrs
    Cards.facultyOfficesTheNightIsStillYoung
    2
    (PerPlayer 2)
    T
    [Circle]

instance HasModifiersFor env FacultyOfficesTheNightIsStillYoung where
  getModifiersFor _ target (FacultyOfficesTheNightIsStillYoung attrs)
    | isTarget attrs target = pure
    $ toModifiers attrs [ Blocked | not (locationRevealed attrs) ]
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env FacultyOfficesTheNightIsStillYoung where
  getActions iid FastPlayerWindow (FacultyOfficesTheNightIsStillYoung attrs@LocationAttrs {..})
    | locationRevealed
    = withBaseActions iid FastPlayerWindow attrs $ pure
      [ ActivateCardAbilityAction
          iid
          (mkAbility
            (toSource attrs)
            1
            (FastAbility
              (GroupClueCost
                (PerPlayer 2)
                (Just $ LocationWithTitle "Faculty Offices")
              )
            )
          )
      ]
  getActions iid window (FacultyOfficesTheNightIsStillYoung attrs) =
    getActions iid window attrs

instance LocationRunner env => RunMessage env FacultyOfficesTheNightIsStillYoung where
  runMessage msg l@(FacultyOfficesTheNightIsStillYoung attrs) = case msg of
    RevealLocation miid lid | lid == locationId attrs -> do
      iid <- maybe getLeadInvestigatorId pure miid
      unshiftMessage $ FindEncounterCard
        iid
        (toTarget attrs)
        (CardMatchByType (EnemyType, singleton Humanoid))
      FacultyOfficesTheNightIsStillYoung <$> runMessage msg attrs
    FoundEncounterCard _iid target card | isTarget attrs target ->
      l <$ unshiftMessage (SpawnEnemyAt (EncounterCard card) (toId attrs))
    UseCardAbility _iid source _ 1 _
      | isSource attrs source && locationRevealed attrs -> l
      <$ unshiftMessage (ScenarioResolution $ Resolution 1)
    _ -> FacultyOfficesTheNightIsStillYoung <$> runMessage msg attrs
