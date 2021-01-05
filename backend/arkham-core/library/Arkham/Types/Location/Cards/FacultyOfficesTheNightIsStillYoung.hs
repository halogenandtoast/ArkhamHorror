module Arkham.Types.Location.Cards.FacultyOfficesTheNightIsStillYoung
  ( facultyOfficesTheNightIsStillYoung
  , FacultyOfficesTheNightIsStillYoung(..)
  )
where

import Arkham.Import

import Arkham.Types.Card.EncounterCardMatcher
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Game.Helpers
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype FacultyOfficesTheNightIsStillYoung = FacultyOfficesTheNightIsStillYoung Attrs
  deriving newtype (Show, ToJSON, FromJSON)

facultyOfficesTheNightIsStillYoung :: FacultyOfficesTheNightIsStillYoung
facultyOfficesTheNightIsStillYoung = FacultyOfficesTheNightIsStillYoung
  $ base { locationVictory = Just 1, locationLabel = "facultyOffices" }
 where
  base = baseAttrs
    "02054"
    (LocationName "Faculty Offices" (Just "The Night is Still Young"))
    EncounterSet.ExtracurricularActivity
    2
    (PerPlayer 2)
    T
    [Circle]
    [Miskatonic]

instance HasModifiersFor env FacultyOfficesTheNightIsStillYoung where
  getModifiersFor _ target (FacultyOfficesTheNightIsStillYoung attrs)
    | isTarget attrs target = pure
    $ toModifiers attrs [ Blocked | not (locationRevealed attrs) ]
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env FacultyOfficesTheNightIsStillYoung where
  getActions iid FastPlayerWindow (FacultyOfficesTheNightIsStillYoung attrs@Attrs {..})
    | locationRevealed
    = withBaseActions iid FastPlayerWindow attrs $ do
      requiredClueCount <- getPlayerCountValue (PerPlayer 2)
      pure
        [ ActivateCardAbilityAction
            iid
            (mkAbility
              (toSource attrs)
              1
              (FastAbility
                (GroupClueCost
                  requiredClueCount
                  (Just $ LocationWithTitle "Faculty Offices")
                )
              )
            )
        ]
  getActions iid window (FacultyOfficesTheNightIsStillYoung attrs) =
    getActions iid window attrs

instance LocationRunner env => RunMessage env FacultyOfficesTheNightIsStillYoung where
  runMessage msg l@(FacultyOfficesTheNightIsStillYoung attrs) = case msg of
    RevealLocation _ lid | lid == locationId attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      unshiftMessage $ FindEncounterCard
        leadInvestigatorId
        (toTarget attrs)
        (EncounterCardMatchByType (EnemyType, Just Humanoid))
      FacultyOfficesTheNightIsStillYoung <$> runMessage msg attrs
    FoundEncounterCard _iid target card | isTarget attrs target ->
      l <$ unshiftMessage (SpawnEnemyAt (EncounterCard card) (locationId attrs))
    UseCardAbility _iid source _ 1
      | isSource attrs source && locationRevealed attrs -> l
      <$ unshiftMessage (Resolution 1)
    _ -> FacultyOfficesTheNightIsStillYoung <$> runMessage msg attrs
