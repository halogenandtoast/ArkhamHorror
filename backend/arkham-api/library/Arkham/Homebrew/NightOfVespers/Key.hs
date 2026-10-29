module Arkham.Homebrew.NightOfVespers.Key where

import Arkham.CampaignLogKey (CampaignLogKey (HomebrewCampaignLogKey), IsCampaignLogKey (..))
import Arkham.Prelude
import Data.Text qualified as T

data NightOfVespersKey
  = FinlayODoyleIsAssistingTheInvestigation
  | GigiGiulianoIsAssistingTheInvestigation
  | KnowsTheNameOfTheRose
  | YouLearnedAboutTheTenebrarumMurders
  | DrArmitageTaughtYouAboutTheThreeMothers
  | YouHadAPropheticDream
  | YouLearnedWhereToFindHelenaNussbaum
  | YouDiscoveredAnaGiulianosConnectionToAmaliaVerdi
  | YouWereInvitedToJoinTheTenebrarumLodge
  | YouReadSgtMandelsFile
  | AnnaKaslowDirectedYouTowardAnOldColleague
  | MaddalenaGuidesYourPath
  | YouVisitedSanFrancisco
  | YouVisitedFreiburgImBreisgau
  | YouVisitedTheGulfOfNaples
  | YouDecidedToBeginTheSearchInTheCityOfFreiburg
  | YouDecidedToBeginTheSearchInTheSchauinsland
  | YouDecidedToBeginTheSearchInTheBlackForest
  | TheInvestigatorsDefeatedTheThreeMothersAndEscapedThePalaceOfSorrows
  | TheInvestigatorsDefeatedANewbornGoddessAndEscapedThePalaceOfSorrows
  | TheInvestigatorsUnboundAFracturedGoddess
  | FinlaySurvived
  | GigiSurvived
  | NaomiOBannionIsAssistingTheInvestigation
  | ABargainWasMade
  | StolasIsUnbound
  deriving stock (Show, Read, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

instance IsCampaignLogKey NightOfVespersKey where
  toCampaignLogKey = HomebrewCampaignLogKey . ("nightOfVespers." <>) . tshow
  fromCampaignLogKey = \case
    HomebrewCampaignLogKey t -> T.stripPrefix "nightOfVespers." t >>= readMay . unpack
    _ -> Nothing
