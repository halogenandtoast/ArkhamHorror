{- | Guide introductions only. Scenario setup reads the remembered lead/start
keys when its board implementation is added; no encounter cards are spawned here.
-}
module Arkham.Homebrew.NightOfVespers.Intros where

import Arkham.Helpers.FlavorText
import Arkham.Homebrew.NightOfVespers.Key
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Scenario.Import.Lifted
import Arkham.ScenarioLogKey (ScenarioLogKey (HomebrewScenarioLogKey))

readIntro :: (HasI18n, ReverseQueue m) => Int -> m ()
readIntro n = flavor $ h "intro.title" >> p ("intro.part" <> tshow n)

introStep :: ReverseQueue m => Int -> m ()
introStep n = doStep n PreScenarioSetup

rememberIntro :: ReverseQueue m => Int -> m ()
rememberIntro n = remember $ HomebrewScenarioLogKey ("nightOfVespers.intro" <> tshow n)

chooseLead :: (HasI18n, ReverseQueue m) => [(Int, NightOfVespersKey)] -> m ()
chooseLead leads = do
  available <- filterM (getHasRecord . snd) leads
  storyWithChooseOneM (h "intro.title" >> p.basic "intro.chooseLead") do
    for_ available \(n, _) -> labeled (tshow n) do
      rememberIntro n
      introStep n

pEffect :: (HasI18n, ReverseQueue m) => Text -> m ()
pEffect key = flavor $ h "intro.title" >> p.basic ("intro.effects." <> key)
