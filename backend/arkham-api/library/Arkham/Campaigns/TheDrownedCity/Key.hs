module Arkham.Campaigns.TheDrownedCity.Key where

import Arkham.Prelude

data TheDrownedCityKey
  = -- | One Last Job
    RubyWonTheBet
  | RubyLostTheBet
  | TheInvestigatorsDiscoveredAnAlienLanguage
  | -- | Tasks (recorded per investigator)
    WalkInFaith
  | ToeTheLine
  | NoPlaceLikeHome
  | GoodMoney
  | DoNoHarm
  | ProveYourWorth
  | DreamsOfDestruction
  | PlumbTheDepths
  | -- | Expedition to R'lyeh
    TheExpeditionHeadedWest
  | TheExpeditionHeadedEast
  | TheExpeditionHelpedThePilgrim
  | TheExpeditionLeftThePilgrim
  | -- | Artifacts Earned
    BarrierNode
  | ObsidianClaw
  | ShardOfYchlecht
  | TidalTablet
  | GrislyMask
  | HorrorInClay
  | -- | The Drowned Quarter
    ThePowerWasDiverted
  | -- | The Grand Vault
    TheInnerSanctumWasUnsealed
  | -- | Interlude III: The Awakening / Return to Arkham
    YourAlliesHaveAPlan
  | TheInvestigatorsStoodTogether
  | -- | The Doom of Arkham
    CthulhuWasDrivenAway
  | CthulhuAnnihilatedTheExpedition
  | CthulhuWasBanished
  deriving stock (Show, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)
