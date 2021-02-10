module Arkham.Types.Location.Cards.OrneLibrary where

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


import qualified Arkham.Types.Action as Action
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype OrneLibrary = OrneLibrary LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

orneLibrary :: OrneLibrary
orneLibrary = OrneLibrary $ base { locationVictory = Just 1 }
 where
  base = baseAttrs
    "02050"
    (Name "Orne Library" Nothing)
    EncounterSet.ExtracurricularActivity
    3
    (PerPlayer 1)
    Triangle
    [Plus, Square]
    [Miskatonic]

instance HasModifiersFor env OrneLibrary where
  getModifiersFor _ target (OrneLibrary attrs) | isTarget attrs target =
    pure $ toModifiers attrs [ActionCostOf (IsAction Action.Investigate) 1]
  getModifiersFor _ (InvestigatorTarget iid) (OrneLibrary attrs)
    | iid `elem` locationInvestigators attrs = pure
    $ toModifiers attrs [ActionCostOf (IsAction Action.Investigate) 1]
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env OrneLibrary where
  getActions i window (OrneLibrary attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env OrneLibrary where
  runMessage msg (OrneLibrary attrs) = OrneLibrary <$> runMessage msg attrs
