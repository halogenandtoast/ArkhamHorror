module Arkham.Types.Location.Cards.OsbornsGeneralStore_207
  ( osbornsGeneralStore_207
  , OsbornsGeneralStore_207(..)
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

newtype OsbornsGeneralStore_207 = OsbornsGeneralStore_207 LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

osbornsGeneralStore_207 :: OsbornsGeneralStore_207
osbornsGeneralStore_207 = OsbornsGeneralStore_207 $ baseAttrs
  "02207"
  (Name "Osborn's General Store" Nothing)
  EncounterSet.BloodOnTheAltar
  3
  (PerPlayer 1)
  Circle
  [Moon, Square]
  [Dunwich]

instance HasModifiersFor env OsbornsGeneralStore_207 where
  getModifiersFor = noModifiersFor

ability :: LocationAttrs -> Ability
ability attrs = mkAbility
  (toSource attrs)
  1
  (ActionAbility Nothing $ Costs [ActionCost 1, ResourceCost 1])

instance ActionRunner env => HasActions env OsbornsGeneralStore_207 where
  getActions iid NonFast (OsbornsGeneralStore_207 attrs)
    | locationRevealed attrs = withBaseActions iid NonFast attrs
    $ pure [ActivateCardAbilityAction iid (ability attrs)]
  getActions iid FastPlayerWindow (OsbornsGeneralStore_207 attrs)
    | locationRevealed attrs = withBaseActions iid FastPlayerWindow attrs $ pure
      [ drawCardUnderneathAction iid attrs
      | iid `on` attrs && locationClues attrs == 0
      ]
  getActions iid window (OsbornsGeneralStore_207 attrs) =
    getActions iid window attrs

instance LocationRunner env => RunMessage env OsbornsGeneralStore_207 where
  runMessage msg l@(OsbornsGeneralStore_207 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ unshiftMessage
        (SearchTopOfDeck iid (InvestigatorTarget iid) 3 [Item] ShuffleBackIn)
    _ -> OsbornsGeneralStore_207 <$> runMessage msg attrs
