{- | The Dark Matter ultimatums, in printed order.

A leaf module by design: core 'Arkham.UltimatumsAndBoons.Types' reads this list
for the catalog, so it must not import engine code. Each one is implemented
where Dark Matter already handles the rule it bends, gated on
'Arkham.UltimatumsAndBoons.hasUltimatumOrBoon'.
-}
module Arkham.Homebrew.DarkMatter.UltimatumDefs where

import Arkham.Homebrew.UltimatumDefs
import Arkham.Prelude

ultimatumCampaign :: Text
ultimatumCampaign = ":dark-matter"

data DarkMatterUltimatum
  = UltimatumOfTheUnspeakableOath
  | UltimatumOfImpendingDoom
  | UltimatumOfTheFeaster
  | UltimatumOfInevitability
  | UltimatumOfTheDarkPast
  | UltimatumOfExploration
  | UltimatumOfAnachronism
  deriving stock (Show, Read, Eq, Ord, Enum, Bounded)

data DarkMatterUltimatums

instance IsHomebrewUltimatums DarkMatterUltimatums where
  homebrewUltimatums =
    campaignUltimatums ultimatumCampaign
      $ map (ultimatum . tshow) [minBound .. maxBound :: DarkMatterUltimatum]
