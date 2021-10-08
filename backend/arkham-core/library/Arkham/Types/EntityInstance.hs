module Arkham.Types.EntityInstance where

import Arkham.Prelude

import Arkham.Types.Asset
import Arkham.Types.Asset.Runner
import Arkham.Types.Card
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.Enemy
import Arkham.Types.Enemy.Runner
import Arkham.Types.Event
import Arkham.Types.Event.Runner
import Arkham.Types.Id
import Arkham.Types.Location
import Arkham.Types.Location.Runner
import Arkham.Types.Matcher (AssetMatcher, EnemyMatcher, LocationMatcher)
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Skill
import Arkham.Types.Skill.Runner
import Arkham.Types.SkillTest
import Arkham.Types.Trait (Trait)
import Arkham.Types.Treachery
import Arkham.Types.Treachery.Runner

data EntityInstance
  = AssetInstance Asset
  | EventInstance Event
  | LocationInstance Location
  | SkillInstance Skill
  | EnemyInstance Enemy
  | TreacheryInstance Treachery

instance EntityInstanceRunner env => RunMessage env EntityInstance where
  runMessage msg (AssetInstance x) = AssetInstance <$> runMessage msg x
  runMessage msg (EnemyInstance x) = EnemyInstance <$> runMessage msg x
  runMessage msg (EventInstance x) = EventInstance <$> runMessage msg x
  runMessage msg (LocationInstance x) = LocationInstance <$> runMessage msg x
  runMessage msg (SkillInstance x) = SkillInstance <$> runMessage msg x
  runMessage msg (TreacheryInstance x) = TreacheryInstance <$> runMessage msg x

instance HasAbilities EntityInstance where
  getAbilities (AssetInstance x) = getAbilities x
  getAbilities (EnemyInstance x) = getAbilities x
  getAbilities (EventInstance x) = getAbilities x
  getAbilities (LocationInstance x) = getAbilities x
  getAbilities (SkillInstance x) = getAbilities x
  getAbilities (TreacheryInstance x) = getAbilities x

toCardInstance :: InvestigatorId -> Card -> EntityInstance
toCardInstance iid card = case toCardType card of
  AssetType -> AssetInstance $ createAsset card
  EncounterAssetType -> AssetInstance $ createAsset card
  EnemyType -> EnemyInstance $ createEnemy card
  EventType -> EventInstance $ createEvent card iid
  LocationType -> LocationInstance $ createLocation card
  PlayerEnemyType -> EnemyInstance $ createEnemy card
  PlayerTreacheryType -> TreacheryInstance $ createTreachery card iid
  SkillType -> SkillInstance $ createSkill card iid
  TreacheryType -> TreacheryInstance $ createTreachery card iid
  ActType -> error "Unhandled"
  AgendaType -> error "Unhandled"
  StoryType -> error "Unhandled"

-- | Masking rules
-- UseCardAbility: Because some abilities have a discard self cost, the card of the ability will have already been discarded when we go to resolve this. While we could use InDiscard in the RunMessage instance for that card's entity, there may be cases where we can trigger abilities without paying the cost, so we want it to be accessible from both.
doNotMask :: Message -> Bool
doNotMask UseCardAbility{} = True
doNotMask _ = False

type EntityInstanceRunner env
  = ( EnemyRunner env
    , LocationRunner env
    , AssetRunner env
    , TreacheryRunner env
    , LocationRunner env
    , SkillRunner env
    , EventRunner env
    )

type SomeEntityHasModifiersFor env
  = ( HasCount ResourceCount env TreacheryId
    , HasCount HorrorCount env InvestigatorId
    , HasId (Maybe OwnerId) env AssetId
    , HasCount ClueCount env LocationId
    , Query AssetMatcher env
    , Query EnemyMatcher env
    , HasPhase env
    , HasSkillTest env
    , HasModifiersFor env ()
    , HasName env AssetId
    , HasId CardCode env EnemyId
    , HasStep AgendaStep env ()
    , HasId InvestigatorId env EventId
    , HasId LocationId env InvestigatorId
    , HasSkillValue env InvestigatorId
    , HasId (Maybe LocationId) env AssetId
    , HasSet CommittedCardId env InvestigatorId
    , HasSet InvestigatorId env LocationId
    , HasSet Trait env LocationId
    , HasSet ConnectedLocationId env LocationId
    , HasCount ClueCount env InvestigatorId
    , HasSet LocationId env ()
    , HasCount ClueCount env EnemyId
    , HasCount CardCount env InvestigatorId
    , HasCount RemainingSanity env InvestigatorId
    , HasCount AssetCount env (InvestigatorId, [Trait])
    , HasSet Trait env AssetId
    , HasCount PlayerCount env ()
    , HasCount ResourceCount env InvestigatorId
    , HasId LocationId env AssetId
    , Query LocationMatcher env
    )

instance SomeEntityHasModifiersFor env => HasModifiersFor env EntityInstance where
  getModifiersFor s t = \case
    AssetInstance a -> getModifiersFor s t a
    EnemyInstance e -> getModifiersFor s t e
    EventInstance e -> getModifiersFor s t e
    LocationInstance l -> getModifiersFor s t l
    TreacheryInstance u -> getModifiersFor s t u
    SkillInstance k -> getModifiersFor s t k
