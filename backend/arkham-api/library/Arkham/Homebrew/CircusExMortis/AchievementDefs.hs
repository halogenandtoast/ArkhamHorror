{- | The Circus Ex Mortis achievement list (campaign guide p38), in printed order.

A leaf module by design: core 'Arkham.Achievement.Types' reads this list for the
catalog and the checklists, so it must not import engine code. The detection
lives in "Arkham.Homebrew.CircusExMortis.Achievements".
-}
module Arkham.Homebrew.CircusExMortis.AchievementDefs where

import Arkham.Homebrew.AchievementDefs
import Arkham.Prelude

achievementCampaign :: Text
achievementCampaign = ":circus-ex-mortis"

data CircusExMortisAchievement
  = Scapegoat
  | MoonlightSonata
  | GravesideChat
  | WolfOfWallStreet
  | DestinedKarma
  | TimeOut
  | WaxAndWane
  | ShootTheMoon
  | UtterLunatic
  | ClownCollege
  | NaturalSelection
  | LactoseIntolerant
  | DeepSleepers
  | PainTrain
  | ViceSquad
  | ManyFutures
  | ManyPasts
  | GOAT
  | StealTheShow
  | GreatestShowOnEarth
  deriving stock (Show, Read, Eq, Ord, Enum, Bounded)

{- | The two "obtain each final version" achievements span playthroughs — a
campaign grants exactly one Amalthea Weaver and one De Cultus Bestiae upgrade —
so they are checklists rather than one-shot earns.
-}
achievementChecklistItems :: CircusExMortisAchievement -> [Text]
achievementChecklistItems = \case
  ManyFutures ->
    ["OracleOfPurity", "OracleOfEnlightenment", "OracleOfResolve", "OracleOfMystery"]
  ManyPasts ->
    ["ProphecyOfTheBeyond", "ProphecyOfTheHorde", "ProphecyOfTheEternal", "ProphecyOfTheBehemoth"]
  _ -> []

data CircusExMortisAchievements

instance IsHomebrewAchievements CircusExMortisAchievements where
  homebrewAchievements =
    campaignAchievements achievementCampaign $ map def [minBound .. maxBound]
   where
    def a = case achievementChecklistItems a of
      [] -> achievement (tshow a)
      items -> checklistAchievement (tshow a) items
