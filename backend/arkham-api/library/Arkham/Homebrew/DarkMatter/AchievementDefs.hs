{- | The Dark Matter achievement list, in printed order.

A leaf module by design: core 'Arkham.Achievement.Types' reads this list for the
catalog, so it must not import engine code. The detection lives in
"Arkham.Homebrew.DarkMatter.Achievements".
-}
module Arkham.Homebrew.DarkMatter.AchievementDefs where

import Arkham.Homebrew.AchievementDefs
import Arkham.Prelude

achievementCampaign :: Text
achievementCampaign = ":dark-matter"

data DarkMatterAchievement
  = Untattered
  | AirlockSequence
  | MentalFortitude
  | BrainBurn
  | OutOfTime
  | SaviorOfNostalgia
  | Neuroscientist
  | SelfHelp
  | Endtimes
  | ItsTooLate
  | LineInTheSky
  | TheHeirToCarcosa
  deriving stock (Show, Read, Eq, Ord, Enum, Bounded)

data DarkMatterAchievements

-- Every entry is finishable in one playthrough, so none of them is a checklist.
instance IsHomebrewAchievements DarkMatterAchievements where
  homebrewAchievements =
    campaignAchievements achievementCampaign
      $ map (achievement . tshow) [minBound .. maxBound :: DarkMatterAchievement]
