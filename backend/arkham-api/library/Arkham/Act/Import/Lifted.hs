module Arkham.Act.Import.Lifted (
  module X,
  module Arkham.Act.Import.Lifted,
)
where

import Arkham.Act.Helpers as X (groupClueCost)
import Arkham.Act.Runner as X (
  ActAttrs (..),
  ActCard,
  ActSide (..),
  AdvancementMethod (..),
  IsAct,
  Message (..),
  ShuffleIn (..),
  act,
  actWith,
  isSide,
  metaL,
  onSide,
  push,
  pushAll,
  sequenceL,
  targetLabel,
  pattern FailedThisSkillTest,
  pattern FailedThisSkillTestBy,
  pattern PassedThisSkillTest,
  pattern R1,
  pattern R2,
  pattern R3,
  pattern R4,
  pattern R5,
  pattern R6,
  pattern R7,
  pattern R8,
  pattern UseThisAbility,
 )
import Arkham.Calculation as X
import Arkham.Classes as X
import Arkham.Cost as X
import Arkham.GameValue as X
import Arkham.Helpers.Log as X (getHasRecord, whenHasRecord)
import Arkham.Helpers.Query as X (getLead, getLeadPlayer, getSetAsideCard)
import Arkham.Id as X
import Arkham.Message.Lifted as X
import Arkham.Prelude as X
import Arkham.Source as X
import Arkham.Target as X
import Arkham.Text as X

import Arkham.Ability.Types
import Arkham.Card.CardDef
import Arkham.Helpers.Act qualified as Msg
import Arkham.Matcher

advanceVia
  :: (ReverseQueue m, EntityId a ~ ActId, Sourceable source, Entity a)
  => AdvancementMethod
  -> a
  -> source
  -> m ()
advanceVia method actId source = push $ Msg.advanceVia method actId source

ifEnemyDefeated :: CardDef -> WindowMatcher
ifEnemyDefeated = ifEnemyDefeatedMatch . enemyIs

ifEnemyDefeatedMatch :: EnemyMatcher -> WindowMatcher
ifEnemyDefeatedMatch = IfEnemyDefeated #after Anyone ByAny

actAbilities
  :: (EntityAttrs act ~ ActAttrs, Entity act) => (ActAttrs -> [Ability]) -> act -> [Ability]
actAbilities = actAbilities' A

actAbilities1
  :: (EntityAttrs act ~ ActAttrs, Entity act) => (ActAttrs -> Ability) -> act -> [Ability]
actAbilities1 = actAbilities1' A

actAbilities'
  :: (EntityAttrs act ~ ActAttrs, Entity act) => ActSide -> (ActAttrs -> [Ability]) -> act -> [Ability]
actAbilities' side abilities (toAttrs -> attrs) = extend attrs $ guard (onSide side attrs) *> abilities attrs

actAbilities1'
  :: (EntityAttrs act ~ ActAttrs, Entity act) => ActSide -> (ActAttrs -> Ability) -> act -> [Ability]
actAbilities1' side ability (toAttrs -> attrs) = extend attrs $ guard (onSide side attrs) *> [ability attrs]
