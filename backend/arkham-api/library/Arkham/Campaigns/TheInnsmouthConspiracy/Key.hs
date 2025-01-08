module Arkham.Campaigns.TheInnsmouthConspiracy.Key where

import Arkham.Prelude

data TheInnsmouthConspiracyKey
  = MemoriesRecovered
  | OutForBlood
  | TheMissionFailed
  | TheMissionWasSuccessful
  | PossibleSuspects
  | PossibleHideouts
  | InnsmouthWasConsumedByTheRisingTide
  | TheInvestigatorsMadeItSafelyToTheirVehicles
  | TheTideHasGrownStronger
  | TheTerrorOfDevilReefIsStillAlive
  | TheTerrorOfDevilReefIsDead
  | TheIdolWasBroughtToTheLighthouse
  | TheMantleWasBroughtToTheLighthouse
  | TheHeaddressWasBroughtToTheLighthouse
  | TheInvestigatorsReachedFalconPointBeforeSunrise
  | TheInvestigatorsReachedFalconPointAfterSunrise
  | PossessesADivingSuit
  | TheInvestigatorsPossessAMapOfYhaNthlei
  | TheInvestigatorsPossessTheKeyToYhaNthlei
  | DagonHasAwakened
  | DagonStillSlumbers
  | TheOrdersRitualWasDisrupted
  | TheGatekeeperHasBeenDefeated
  | TheGuardianOfYhanthleiIsDispatched
  | TheGatewayToYhanthleiRecognizesYouAsTheRightfulKeeper
  | TheInvestigatorsEscapedYhanthlei
  | ThePlotOfTheDeepOnesWasThwarted
  | TheFloodHasBegun
  | AgentHarpersMissionIsComplete
  | TheRichesOfTheDeepAreLostForever
  | AgentHarpersMissionIsCompleteButAtWhatCost
  | TheRichesOfTheDeepAreLostForeverButAtWhatCost
  | TheDeepOnesHaveFloodedTheEarth
  | TheHorribleTruth
  deriving stock (Show, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)
