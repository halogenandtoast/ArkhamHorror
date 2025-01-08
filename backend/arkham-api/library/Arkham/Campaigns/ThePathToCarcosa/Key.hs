module Arkham.Campaigns.ThePathToCarcosa.Key where

import Arkham.Prelude

data ThePathToCarcosaKey
  = TheStrangerIsOnToYou
  | ChasingTheStranger
  | YouTriedToWarnThePolice
  | ThePoliceAreSuspiciousOfYou
  | YouChoseNotToGoToThePolice
  | Doubt
  | Conviction
  | VIPsInterviewed
  | VIPsSlain
  | YouIntrudedOnASecretMeeting
  | YouFledTheDinnerParty
  | YouSlayedTheMonstersAtTheDinnerParty
  | YouTookTheOnyxClasp
  | YouLeftTheOnyxClaspBehind
  | YouDestroyedTheOathspeaker
  | TheFollowersOfTheSignHaveFoundTheWayForward
  | TheKingClaimedItsVictims
  | TheInvestigatorsWereAttackedAsTheyEscapedTheAsylum
  | TheInvestigatorsEscapedTheAsylum
  | YouIgnoredDanielsWarning
  | YouHeadedDanielsWarning
  | YouDidNotEscapeTheGazeOfThePhantom
  | YouFoundNigelsHome
  | YouFoundNigelEngram
  | YouWereUnableToFindNigel
  | YouAwokeInsideTheCatacombs
  | YouEnteredTheCatacombsOnYourOwn
  | YouKnowTheSiteOfTheGate
  | ReadActII
  | YouOpenedThePathBelow
  | YouOpenedThePathAbove
  | TheRealmOfCarcosaMergedWithOurOwnAndHasturRulesOverThemBoth
  | TheInvestigatorsPreventedHasturFromEscapingHisPrison
  | HasturHasYouInHisGrasp
  | Possessed
  deriving stock (Show, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)
