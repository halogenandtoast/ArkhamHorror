module Arkham.Types.Location.Cards.Dormitories where

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
