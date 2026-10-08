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
  | {- | Recorded under "Memories Recovered" now, as the Campaign Log prints it.
    Kept so campaigns that finished before the move still decode.
    -}
    TheHorribleTruth
  deriving stock (Show, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)
