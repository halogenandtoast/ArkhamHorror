module Arkham.Types.Location.Cards.Dormitories where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.Resolution
import Arkham.Types.Trait
import Arkham.Types.Window

newtype Dormitories = Dormitories LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dormitories :: LocationId -> Dormitories
dormitories lid = Dormitories $ base { locationVictory = Just 1 }
 where
  base = baseAttrs
    lid
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

ability :: LocationAttrs -> Ability
ability attrs = mkAbility
  (toSource attrs)
  1
  (FastAbility
    (GroupClueCost (PerPlayer 3) $ Just (LocationWithTitle "Dormitories"))
  )

instance ActionRunner env => HasActions env Dormitories where
  getActions iid FastPlayerWindow (Dormitories attrs@LocationAttrs {..})
    | locationRevealed = withBaseActions iid FastPlayerWindow attrs
    $ pure [ActivateCardAbilityAction iid (ability attrs)]
  getActions iid window (Dormitories attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env Dormitories where
  runMessage msg l@(Dormitories attrs) = case msg of
    UseCardAbility _iid source _ 1 _
      | isSource attrs source && locationRevealed attrs -> l
      <$ unshiftMessage (ScenarioResolution $ Resolution 2)
    _ -> Dormitories <$> runMessage msg attrs
