module Arkham.Types.EntityInstance
  ( module Arkham.Types.EntityInstance
  ) where

import Arkham.Prelude

import Arkham.Types.Asset
import Arkham.Types.Asset.Runner
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Enemy
import Arkham.Types.Enemy.Runner
import Arkham.Types.Event
import Arkham.Types.Event.Runner
import Arkham.Types.Location
import Arkham.Types.Location.Runner
import Arkham.Types.Message
import Arkham.Types.Skill
import Arkham.Types.Skill.Runner
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

-- UseCardAbility and Revelation are special and need access to the original instance
-- therefor we do not mask with In{Hand,Discard,etc.}
doNotMask :: Message -> Bool
doNotMask UseCardAbility{} = True
doNotMask Revelation{} = True
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
