module Arkham.Types.Location.Cards.ParlorCar
  ( parlorCar
  , ParlorCar(..)
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
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype ParlorCar = ParlorCar LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

parlorCar :: ParlorCar
parlorCar = ParlorCar $ base
  { locationConnectsTo = setFromList [LeftOf, RightOf]
  , locationVictory = Just 1
  }
 where
  base = baseAttrs
    "02174"
    (Name "Parlor Car" Nothing)
    EncounterSet.TheEssexCountyExpress
    3
    (PerPlayer 1)
    NoSymbol
    []
    (singleton Train)

instance HasCount ClueCount env LocationId => HasModifiersFor env ParlorCar where
  getModifiersFor _ target (ParlorCar location@LocationAttrs {..})
    | isTarget location target = case lookup LeftOf locationDirections of
      Just leftLocation -> do
        clueCount <- unClueCount <$> getCount leftLocation
        pure $ toModifiers
          location
          (CannotInvestigate
          : [ Blocked | not locationRevealed && clueCount > 0 ]
          )
      Nothing -> pure $ toModifiers location [CannotInvestigate]
  getModifiersFor _ _ _ = pure []

ability :: LocationAttrs -> Ability
ability attrs = mkAbility
  (toSource attrs)
  1
  (ActionAbility Nothing $ Costs [ActionCost 1, ResourceCost 3])

instance ActionRunner env => HasActions env ParlorCar where
  getActions iid NonFast (ParlorCar attrs) | locationRevealed attrs =
    withBaseActions iid NonFast attrs
      $ pure [ ActivateCardAbilityAction iid (ability attrs) | iid `on` attrs ]
  getActions iid window (ParlorCar attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env ParlorCar where
  runMessage msg l@(ParlorCar attrs@LocationAttrs {..}) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ unshiftMessage (DiscoverCluesAtLocation iid locationId 1 Nothing)
    _ -> ParlorCar <$> runMessage msg attrs
