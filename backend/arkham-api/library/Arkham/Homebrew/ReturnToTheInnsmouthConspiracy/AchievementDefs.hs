{- | The (Unofficial) Return to The Innsmouth Conspiracy achievement list, in printed
order. The box prints its own list, so a Return to game earns these instead of the
official campaign's -- those are gated to campaign "07" and so never fire here.

A leaf module by design: core 'Arkham.Achievement.Types' reads this list for the catalog,
so it must not import engine code. The detection lives in
"Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Achievements".
-}
module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.AchievementDefs where

import Arkham.Homebrew.AchievementDefs
import Arkham.Prelude

achievementCampaign :: Text
achievementCampaign = ":return-to-the-innsmouth-conspiracy"

data ReturnToTheInnsmouthConspiracyAchievement
  = AFullBag
  | ThisGuyAgain
  | OffToAGoodStart
  | FriendOfThePeople
  | RunForYourLives
  | GoodSwimmer
  | StepOnIt
  | WayTooCute
  | MakingYourOwnLuck
  | GoBackToSleep
  | ActuallyJustHereForTheMoney
  | IDoNotRecallThisPlace
  | TotalRecall
  | SomethingSmellsFishy
  | ExpertFisherman
  deriving stock (Show, Read, Eq, Ord, Enum, Bounded)

data ReturnToTheInnsmouthConspiracyAchievements

instance IsHomebrewAchievements ReturnToTheInnsmouthConspiracyAchievements where
  homebrewAchievements =
    campaignAchievements achievementCampaign
      $ map (achievement . tshow) [minBound @ReturnToTheInnsmouthConspiracyAchievement ..]
