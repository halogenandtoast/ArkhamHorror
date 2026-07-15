module Arkham.Campaigns.TheDreamEaters.FlavorText where

import Arkham.I18n
import Arkham.Prelude
import Arkham.Text

campaignFlavor :: Text -> FlavorText
campaignFlavor key = withI18n $ i18n $ "theDreamEaters." <> key

campaignFlavorWithTitle :: Text -> FlavorText
campaignFlavorWithTitle key = withI18n $ i18nWithTitle $ "theDreamEaters." <> key

prologue :: FlavorText
prologue = campaignFlavorWithTitle "prologue"

theBlackCat1 :: FlavorText
theBlackCat1 = campaignFlavorWithTitle "theBlackCat1"

theBlackCat2 :: FlavorText
theBlackCat2 = campaignFlavorWithTitle "theBlackCat2"

theBlackCat3 :: FlavorText
theBlackCat3 = campaignFlavorWithTitle "theBlackCat3"

youAreOnYourOwn :: FlavorText
youAreOnYourOwn = campaignFlavor "youAreOnYourOwn"

theBlackCatSharedKnowledgeOfTheDreamlands :: FlavorText
theBlackCatSharedKnowledgeOfTheDreamlands = campaignFlavor "theBlackCatSharedKnowledgeOfTheDreamlandsStory"

theBlackCatDeliveredNewsOfYourPlight :: FlavorText
theBlackCatDeliveredNewsOfYourPlight = campaignFlavor "theBlackCatDeliveredNewsOfYourPlight"

theBlackCatWarnedTheOthers :: FlavorText
theBlackCatWarnedTheOthers = campaignFlavor "theBlackCatWarnedTheOthersStory"

okayFineHaveItYourWayThen :: FlavorText
okayFineHaveItYourWayThen = campaignFlavor "okayFineHaveItYourWayThen"

theOneironauts1 :: FlavorText
theOneironauts1 = campaignFlavorWithTitle "theOneironauts1"

where'sBlondie :: FlavorText
where'sBlondie = campaignFlavor "wheresBlondie"

youAskedForIt :: FlavorText
youAskedForIt = campaignFlavor "youAskedForIt"

youDidNotAskForIt :: FlavorText
youDidNotAskForIt = campaignFlavor "youDidNotAskForIt"

theOneironauts2 :: FlavorText
theOneironauts2 = campaignFlavor "theOneironauts2"

atLeastOneNotCaptured :: FlavorText
atLeastOneNotCaptured = campaignFlavor "atLeastOneNotCaptured"

allCaptured :: FlavorText
allCaptured = campaignFlavor "allCaptured"

searchingForTheTruth :: FlavorText
searchingForTheTruth = campaignFlavor "searchingForTheTruth"

nowWhereWasI :: FlavorText
nowWhereWasI = campaignFlavor "nowWhereWasI"

theBlackCatRequestedAidFromTheOthers :: FlavorText
theBlackCatRequestedAidFromTheOthers = campaignFlavor "theBlackCatRequestedAidFromTheOthers"

warnedTheOthersStory :: FlavorText
warnedTheOthersStory = campaignFlavor "warnedTheOthersStory"

sharedTheKnowledgeStory :: FlavorText
sharedTheKnowledgeStory = campaignFlavor "sharedTheKnowledgeStory"

theGreatOnes1 :: FlavorText
theGreatOnes1 = campaignFlavorWithTitle "theGreatOnes1"

theGreatOnes1GrowWeaker :: FlavorText
theGreatOnes1GrowWeaker = campaignFlavor "theGreatOnes1GrowWeaker"

theGreatOnes1Searching :: FlavorText
theGreatOnes1Searching = campaignFlavor "theGreatOnes1Searching"

theGreatOnes1YouAskedForIt :: FlavorText
theGreatOnes1YouAskedForIt = campaignFlavor "theGreatOnes1YouAskedForIt"

theGreatOnes1Part2 :: FlavorText
theGreatOnes1Part2 = campaignFlavor "theGreatOnes1Part2"

theGreatOnes2 :: FlavorText
theGreatOnes2 = campaignFlavorWithTitle "theGreatOnes2"

theGreatOnes2TheSilverKey :: FlavorText
theGreatOnes2TheSilverKey = campaignFlavor "theGreatOnes2TheSilverKey"

theGreatOnes2Searching :: FlavorText
theGreatOnes2Searching = campaignFlavor "theGreatOnes2Searching"

theGreatOnes2Part2 :: FlavorText
theGreatOnes2Part2 = campaignFlavorWithTitle "theGreatOnes2Part2"

theGreatOnes2Nyarlathotep :: FlavorText
theGreatOnes2Nyarlathotep = campaignFlavor "theGreatOnes2Nyarlathotep"

theGreatOnes2AtlachNacha :: FlavorText
theGreatOnes2AtlachNacha = campaignFlavor "theGreatOnes2AtlachNacha"
