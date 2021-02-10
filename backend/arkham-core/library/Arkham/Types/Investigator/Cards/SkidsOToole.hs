module Arkham.Types.Investigator.Cards.SkidsOToole
  ( SkidsOToole(..)
  , skidsOToole
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


import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Stats
import Arkham.Types.Trait

newtype SkidsOToole = SkidsOToole InvestigatorAttrs
  deriving newtype (Show, ToJSON, FromJSON, Entity)

instance HasModifiersFor env SkidsOToole where
  getModifiersFor source target (SkidsOToole attrs) =
    getModifiersFor source target attrs

skidsOToole :: SkidsOToole
skidsOToole = SkidsOToole $ baseAttrs
  "01003"
  "\"Skids\" O'Toole"
  Rogue
  Stats
    { health = 8
    , sanity = 6
    , willpower = 2
    , intellect = 3
    , combat = 3
    , agility = 4
    }
  [Criminal]

ability :: InvestigatorAttrs -> Ability
ability attrs = base { abilityLimit = PlayerLimit PerTurn 1 }
  where base = mkAbility (toSource attrs) 1 (FastAbility $ ResourceCost 2)

instance ActionRunner env => HasActions env SkidsOToole where
  getActions iid (DuringTurn You) (SkidsOToole a@InvestigatorAttrs {..})
    | iid == investigatorId = pure [ActivateCardAbilityAction iid (ability a)]
  getActions _ _ _ = pure []

instance HasTokenValue env SkidsOToole where
  getTokenValue (SkidsOToole attrs) iid ElderSign
    | iid == investigatorId attrs = pure
    $ TokenValue ElderSign (PositiveModifier 2)
  getTokenValue (SkidsOToole attrs) iid token = getTokenValue attrs iid token

instance (InvestigatorRunner env) => RunMessage env SkidsOToole where
  runMessage msg i@(SkidsOToole attrs@InvestigatorAttrs {..}) = case msg of
    UseCardAbility _ (InvestigatorSource iid) _ 1 _ | iid == investigatorId ->
      pure . SkidsOToole $ attrs & remainingActionsL +~ 1
    PassedSkillTest iid _ _ (DrawnTokenTarget token) _ _
      | iid == investigatorId && drawnTokenFace token == ElderSign -> i
      <$ unshiftMessage (TakeResources iid 2 False)
    _ -> SkidsOToole <$> runMessage msg attrs
