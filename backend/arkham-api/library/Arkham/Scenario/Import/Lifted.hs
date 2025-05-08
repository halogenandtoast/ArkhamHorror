module Arkham.Scenario.Import.Lifted (module X, module Arkham.Scenario.Import.Lifted) where

import Arkham.CampaignLogKey as X
import Arkham.ChaosToken as X
import Arkham.Classes as X
import Arkham.Difficulty as X
import Arkham.Helpers.Log as X (getHasRecord)
import Arkham.Helpers.Message as X (
  pattern R1,
  pattern R2,
  pattern R3,
  pattern R4,
  pattern R5,
  pattern R6,
  pattern R7,
  pattern R8,
 )
import Arkham.Helpers.Scenario as X
import Arkham.Message as X (
  Message (..),
  ShuffleIn (..),
  pattern FailedSkillTestWithToken,
  pattern PassedSkillTestWithToken,
 )
import Arkham.Message.Lifted as X hiding (setActDeck, setActDeckN, setAgendaDeck, setAgendaDeckN)
import Arkham.Prelude as X
import Arkham.Question as X
import Arkham.Scenario.Runner as X (
  IsScenario,
  ScenarioAttrs,
  additionalReferencesL,
  decksL,
  decksLayoutL,
  metaL,
  push,
  pushAll,
  pushWhen,
  referenceL,
  scenario,
  scenarioTimesPlayed,
  scenarioWith,
  sideStory,
  standaloneCampaignLogL,
 )
import Arkham.Scenario.Setup as X
import Arkham.Source as X
import Arkham.Target as X
import Arkham.Text as X

import Arkham.I18n

resolutionWithXp :: (HasI18n, ReverseQueue m) => Scope -> m Int -> m ()
resolutionWithXp s f = do
  xp <- f
  story $ withVars ["xp" .= xp] $ i18nWithTitle s

resolution :: (HasI18n, ReverseQueue m) => Scope -> m ()
resolution = story . i18nWithTitle
