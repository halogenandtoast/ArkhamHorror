module Arkham.Types.Investigator.Cards.RolandBanks
  ( RolandBanks(..)
  , rolandBanks
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


import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Stats
import Arkham.Types.Trait

newtype RolandBanks = RolandBanks InvestigatorAttrs
  deriving newtype (Show, ToJSON, FromJSON, Entity)

instance HasModifiersFor env RolandBanks where
  getModifiersFor source target (RolandBanks attrs) =
    getModifiersFor source target attrs

rolandBanks :: RolandBanks
rolandBanks = RolandBanks
  $ baseAttrs "01001" "Roland Banks" Guardian stats [Agency, Detective]
 where
  stats = Stats
    { health = 9
    , sanity = 5
    , willpower = 3
    , intellect = 3
    , combat = 4
    , agility = 2
    }

ability :: InvestigatorAttrs -> Ability
ability attrs = base { abilityLimit = PlayerLimit PerRound 1 }
  where base = mkAbility (toSource attrs) 1 (ReactionAbility Free)

instance ActionRunner env => HasActions env RolandBanks where
  getActions iid (WhenEnemyDefeated You) (RolandBanks a) | iid == toId a = do
    clueCount <- unClueCount <$> getCount (investigatorLocation a)
    pure [ ActivateCardAbilityAction iid (ability a) | clueCount > 0 ]
  getActions _ _ _ = pure []

instance HasCount ClueCount env LocationId => HasTokenValue env RolandBanks where
  getTokenValue (RolandBanks attrs) iid ElderSign | iid == toId attrs = do
    locationClueCount <- unClueCount <$> getCount (investigatorLocation attrs)
    pure $ TokenValue ElderSign (PositiveModifier locationClueCount)
  getTokenValue (RolandBanks attrs) iid token = getTokenValue attrs iid token

instance InvestigatorRunner env => RunMessage env RolandBanks where
  runMessage msg rb@(RolandBanks attrs@InvestigatorAttrs {..}) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      rb <$ unshiftMessage
        (DiscoverCluesAtLocation (toId attrs) investigatorLocation 1 Nothing)
    _ -> RolandBanks <$> runMessage msg attrs
