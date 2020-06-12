module Arkham.Api.Handler.Games where

import Import

data ArkhamCycle = NightOfTheZealot | TheDunwichLegacy
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data ArkhamScenario = ScenarioOne | ScenarioTwo
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data ArkhamInvestigator = ArkhamInvestigator
  { investigatorName :: Text
  , investigatorImage :: Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data ArkhamCard = ArkhamCard
  { cost :: Maybe Int
  , image :: Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data ArkhamPlayer = ArkhamPlayer
  { investigator :: ArkhamInvestigator
  , sanityDamage :: Int
  , healthDamage :: Int
  , resources :: Int
  , hand :: [ArkhamCard]
  , inPlay :: [ArkhamCard]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data ArkhamGame = ArkhamGame
  { cycle :: ArkhamCycle
  , scenario :: ArkhamScenario
  , player :: ArkhamPlayer
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

getApiV1ArkhamGameR :: Int -> Handler ArkhamGame
getApiV1ArkhamGameR _ = pure $ ArkhamGame NightOfTheZealot ScenarioOne player
 where
  player = ArkhamPlayer rolandBanks 0 0 5 [machete] []
  machete = ArkhamCard (Just 3) "https://arkhamdb.com/bundles/cards/01020.png"
  rolandBanks = ArkhamInvestigator "Roland Banks" "https://arkhamdb.com/bundles/cards/01001.png"
