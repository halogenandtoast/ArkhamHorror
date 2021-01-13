module Arkham.Types.Location.Cards.Dormitories where

import Arkham.Import

import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Game.Helpers
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype Dormitories = Dormitories Attrs
  deriving newtype (Show, ToJSON, FromJSON)

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

ability :: Int -> Attrs -> Ability
ability requiredClueCount attrs = mkAbility
  (toSource attrs)
  1
  (FastAbility
    (GroupClueCost requiredClueCount $ Just (LocationWithTitle "Dormitories"))
  )

instance ActionRunner env => HasActions env Dormitories where
  getActions iid FastPlayerWindow (Dormitories attrs@Attrs {..})
    | locationRevealed = withBaseActions iid FastPlayerWindow attrs $ do
      requiredClueCount <- getPlayerCountValue (PerPlayer 3)
      pure [ActivateCardAbilityAction iid (ability requiredClueCount attrs)]
  getActions iid window (Dormitories attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env Dormitories where
  runMessage msg l@(Dormitories attrs) = case msg of
    UseCardAbility _iid source _ 1 _
      | isSource attrs source && locationRevealed attrs -> l
      <$ unshiftMessage (Resolution 2)
    _ -> Dormitories <$> runMessage msg attrs
