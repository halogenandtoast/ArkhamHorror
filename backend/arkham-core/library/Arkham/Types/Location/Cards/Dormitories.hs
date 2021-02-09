module Arkham.Types.Location.Cards.Dormitories where


import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Game.Helpers
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype Dormitories = Dormitories LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dormitories :: Dormitories
dormitories = Dormitories $ base { locationVictory = Just 1 }
 where
  base = baseAttrs
    "02052"
    (Name "Dormitories" Nothing)
    EncounterSet.ExtracurricularActivity
    1
    (PerPlayer 3)
    Equals
    [Diamond]
    [Miskatonic]

instance HasModifiersFor env Dormitories where
  getModifiersFor _ target (Dormitories attrs) | isTarget attrs target =
    pure $ toModifiers attrs [ Blocked | not (locationRevealed attrs) ]
  getModifiersFor _ _ _ = pure []

ability :: Int -> LocationAttrs -> Ability
ability requiredClueCount attrs = mkAbility
  (toSource attrs)
  1
  (FastAbility
    (GroupClueCost requiredClueCount $ Just (LocationWithTitle "Dormitories"))
  )

instance ActionRunner env => HasActions env Dormitories where
  getActions iid FastPlayerWindow (Dormitories attrs@LocationAttrs {..})
    | locationRevealed = withBaseActions iid FastPlayerWindow attrs $ do
      requiredClueCount <- getPlayerCountValue (PerPlayer 3)
      pure [ActivateCardAbilityAction iid (ability requiredClueCount attrs)]
  getActions iid window (Dormitories attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env Dormitories where
  runMessage msg l@(Dormitories attrs) = case msg of
    UseCardAbility _iid source _ 1 _
      | isSource attrs source && locationRevealed attrs -> l
      <$ unshiftMessage (ScenarioResolution $ Resolution 2)
    _ -> Dormitories <$> runMessage msg attrs
