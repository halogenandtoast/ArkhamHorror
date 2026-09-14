module Arkham.Scenarios.TheDreamEaters.TheSearchForKadath.Helpers where

import Arkham.Campaigns.TheDreamEaters.Helpers
import Arkham.Classes.HasGame
import Arkham.Helpers.FlavorText (FlavorTextBuilder, h, p, setTitle)
import Arkham.Helpers.Log
import Arkham.I18n
import Arkham.Prelude
import Arkham.ScenarioLogKey

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "theSearchForKadath" a

scenarioFlavorText :: Scope -> FlavorTextBuilder ()
scenarioFlavorText entry = scenarioI18n $ scope "flavorText" $ scope entry do
  setTitle "title"
  p "body"

{- | The scenario's opening passage, headed by the scenario's own title the way
every other scenario intro is.
-}
scenarioIntroText :: Scope -> FlavorTextBuilder ()
scenarioIntroText entry = scenarioI18n do
  scope "intro" $ h "title"
  scope "flavorText" $ scope entry $ p "body"

getSignsOfTheGods :: HasGame m => m Int
getSignsOfTheGods = scenarioCount SignOfTheGods

data Region = Oriab | Mnar | ForbiddenLands | TimelessRealm
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype Meta = Meta {regions :: [Region]}
  deriving stock Generic
  deriving anyclass (ToJSON, FromJSON)
