module Arkham.Types.EntityInstance
  ( module Arkham.Types.EntityInstance
  )
where

import Arkham.Prelude

import Arkham.Types.Asset
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Enemy
import Arkham.Types.Event
import Arkham.Types.Investigator.Runner
import Arkham.Types.InvestigatorId
import Arkham.Types.Location
import Arkham.Types.Message
import Arkham.Types.Skill
import Arkham.Types.SkillTest
import Arkham.Types.Treachery

data EntityInstance
  = AssetInstance Asset
  | EventInstance Event
  | LocationInstance Location
  | SkillInstance Skill
  | EnemyInstance Enemy
  | TreacheryInstance Treachery

instance InvestigatorRunner env => RunMessage env EntityInstance where
  runMessage msg (AssetInstance x) = AssetInstance <$> runMessage msg x
  runMessage msg (EnemyInstance x) = EnemyInstance <$> runMessage msg x
  runMessage msg (EventInstance x) = EventInstance <$> runMessage msg x
  runMessage msg (LocationInstance x) = LocationInstance <$> runMessage msg x
  runMessage msg (SkillInstance x) = SkillInstance <$> runMessage msg x
  runMessage msg (TreacheryInstance x) = TreacheryInstance <$> runMessage msg x

instance (ActionRunner env, HasSkillTest env) => HasActions env EntityInstance where
  getActions iid window (AssetInstance x) = getActions iid window x
  getActions iid window (EnemyInstance x) = getActions iid window x
  getActions iid window (EventInstance x) = getActions iid window x
  getActions iid window (LocationInstance x) = getActions iid window x
  getActions iid window (SkillInstance x) = getActions iid window x
  getActions iid window (TreacheryInstance x) = getActions iid window x

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

-- UseCardAbility and Revelation are special and need access to the original instance
-- therefor we do not mask with In{Hand,Discard,etc.}
doNotMask :: Message -> Bool
doNotMask UseCardAbility{} = True
doNotMask Revelation{} = True
doNotMask _ = False

