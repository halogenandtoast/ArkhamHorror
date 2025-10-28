module Arkham.Agenda.Import.Lifted (module Arkham.Agenda.Import.Lifted, module X) where

import Arkham.Agenda.Runner as X (
  AgendaAttrs (..),
  AgendaCard,
  AgendaSide (..),
  IsAgenda,
  agenda,
  agendaWith,
  doomL,
  doomThresholdL,
  is,
  isSide,
  metaL,
  onSide,
  push,
  pushAll,
  pushM,
  removeDoomMatchersL,
  pattern FailedThisSkillTest,
  pattern FailedThisSkillTestBy,
  pattern R1,
  pattern R2,
  pattern R3,
  pattern R4,
  pattern R5,
  pattern R6,
  pattern R7,
  pattern R8,
 )
import Arkham.Calculation as X
import Arkham.Classes as X
import Arkham.GameValue as X
import Arkham.Id as X
import Arkham.Message as X (
  AgendaAdvancementMethod (..),
  CanAdvance (..),
  Message (..),
  ShuffleIn (..),
  pattern AdvanceAgenda,
  pattern PassedThisSkillTest,
  pattern RemoveClues,
  pattern UseThisAbility,
 )
import Arkham.Message.Lifted as X
import Arkham.Prelude as X
import Arkham.Question as X
import Arkham.Source as X
import Arkham.Target as X

import Arkham.Card.CardDef
import Arkham.Matcher
import Arkham.Resolution

advanceAgenda :: ReverseQueue m => AgendaAttrs -> m ()
advanceAgenda attrs = push $ AdvanceAgenda attrs.id

noResolution :: ReverseQueue m => m ()
noResolution = push $ ScenarioResolution NoResolution

advanceToAgenda :: ReverseQueue m => AgendaAttrs -> CardDef -> AgendaSide -> m ()
advanceToAgenda attrs newAgenda side = push $ AdvanceToAgenda attrs.deck newAgenda side (toSource attrs)

setMeta :: ToJSON a => a -> AgendaAttrs -> AgendaAttrs
setMeta meta attrs = attrs & metaL .~ toJSON meta

ifEnemyDefeated :: CardDef -> WindowMatcher
ifEnemyDefeated = ifEnemyDefeatedMatch . enemyIs

ifEnemyDefeatedMatch :: EnemyMatcher -> WindowMatcher
ifEnemyDefeatedMatch = IfEnemyDefeated #after Anyone ByAny
