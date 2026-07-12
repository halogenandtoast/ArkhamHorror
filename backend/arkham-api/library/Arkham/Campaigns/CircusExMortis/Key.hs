module Arkham.Campaigns.CircusExMortis.Key where

import Arkham.Prelude

data CircusExMortisKey
  = -- | One Night Only
    TheRingmasterDoesNotSuspectYou
  | TheRingmasterHasHisEyeOnYou
  | -- | The Primrose Path
    TheInvestigatorsWereLostInTheArkhamWoods
  | TheInvestigatorsBypassedTheIllusions
  | -- | Harm's Way (recorded count: X groups of citizens were saved)
    GroupsOfCitizensWereSavedFromTheCircus
  | -- | All Points West (recorded count: the ringmaster had X days to prepare)
    TheRingmasterHadDaysToPrepare
  | -- | Piper at the Gates of Dawn
    TheInvestigatorsCouldNotEscapeTheCircus
  | TheInvestigatorsClashedWithBlake
  | TheInvestigatorsUnmaskedBlake
  | TheInvestigatorsStruckDownBlake
  | -- | Bacchanalia
    TheInvestigatorsMustFollowTheCult
  | TheInvestigatorsDiscoveredTheRitualsLocation
  | -- | Red Sunrise
    TheInvestigatorsDidNotArriveInTime
  | TheCultRallies
  | -- | Thousand to One
    TheInvestigatorsWereDevouredByTheThousandYoung
  | TheInvestigatorsEndedTheRitual
  | ShubNiggurathReignsOverAnEclipsedWorld
  | ShubNiggurathVanishedWithTheEclipse
  | -- | Recorded by The Prophecy Fulfilled / The Prophecy Unfulfilled (Thousand to One)
    TheProphecyWasFulfilled
  | TheProphecyWasUnfulfilled
  | -- | Recorded set: the Fata Diana destinies ("The one who will strike the
    -- heart", ...). Scenario VIII finds the story card matching each recorded
    -- destiny. Record the destiny story card's card code.
    Destinies
  | -- | Epilogue
    TheNewMoonCircusWasNeverSeenAgain
  | TheNewMoonCircusMaySomedayReturn
  deriving stock (Show, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)
