module Arkham.Types.Location.Cards.SecurityOffice_129
  ( securityOffice_129
  , SecurityOffice_129(..)
  ) where

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

newtype SecurityOffice_129 = SecurityOffice_129 LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

securityOffice_129 :: SecurityOffice_129
securityOffice_129 = SecurityOffice_129 $ baseAttrs
  "02129"
  (Name "Security Office" Nothing)
  EncounterSet.TheMiskatonicMuseum
  3
  (PerPlayer 2)
  Diamond
  [Square]
  (singleton Miskatonic)

instance HasModifiersFor env SecurityOffice_129 where
  getModifiersFor = noModifiersFor

ability :: LocationAttrs -> Ability
ability attrs =
  (mkAbility (toSource attrs) 1 (ActionAbility Nothing $ ActionCost 2))
    { abilityLimit = PlayerLimit PerTurn 1
    }

instance ActionRunner env => HasActions env SecurityOffice_129 where
  getActions iid NonFast (SecurityOffice_129 attrs) =
    withBaseActions iid NonFast attrs
      $ pure [ ActivateCardAbilityAction iid (ability attrs) | iid `on` attrs ]
  getActions iid window (SecurityOffice_129 attrs) =
    getActions iid window attrs

instance LocationRunner env => RunMessage env SecurityOffice_129 where
  runMessage msg l@(SecurityOffice_129 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      unrevealedExhibitHalls <- map unUnrevealedLocationId
        <$> getSetList (LocationWithTitle "ExhibitHall")
      l <$ unshiftMessage
        (chooseOne
          iid
          (TargetLabel
              ScenarioDeckTarget
              [LookAtTopOfDeck iid ScenarioDeckTarget 1]
          : [ LookAtRevealed exhibitHall
            | exhibitHall <- unrevealedExhibitHalls
            ]
          )
        )
    _ -> SecurityOffice_129 <$> runMessage msg attrs
