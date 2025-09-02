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
import Arkham.Helpers.Scenario as X hiding (getIsReturnTo)
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
  actStackL,
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
import Arkham.Helpers.FlavorText
import Arkham.Id
import Arkham.Matcher.Investigator

resolutionWithXp :: (HasI18n, ReverseQueue m) => Scope -> m Int -> m ()
resolutionWithXp s f = do
  xp <- f
  resolutionFlavor $ withVars ["xp" .= xp] $ setTitle (s <> ".title") >> p (s <> ".body")

resolution :: (HasI18n, ReverseQueue m) => Scope -> m ()
resolution s = resolutionFlavor $ setTitle (s <> ".title") >> p (s <> ".body")

eachUnresigned :: ReverseQueue m => (InvestigatorId -> m ()) -> m ()
eachUnresigned = selectEach (IncludeEliminated $ not_ ResignedInvestigator)
