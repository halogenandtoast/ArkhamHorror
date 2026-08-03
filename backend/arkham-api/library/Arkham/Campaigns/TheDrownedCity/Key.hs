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
  | -- | The Inescapable, which stalks the expedition across scenarios
    TheCreatureWasDefeated
  | -- | Artifacts Earned
    BarrierNode
  | ObsidianClaw
  | ShardOfYchlecht
  | TidalTablet
  | GrislyMask
  | HorrorInClay
  | -- | The Apiary
    ThePilgrimsWereSaved
  | ThePilgrimsWereDevoured
  | TheInvestigatorsExterminatedTheAlienParasites
  | -- | The Drowned Quarter
    ThePowerWasDiverted
  | -- | The Grand Vault
    TheInnerSanctumWasUnsealed
  | {- | Obsidian Canyons, Prove Your Worth. Two halves of one "Remember" note:
    who tied the ropes, and who they chose to help.
    -}
    HelpedWithTheRopes
  | WasHelpedWithTheRopes
  | -- | Alien Glyphs (translated glyph record; recorded-set of rune letters "A".."Z")
    DiscoveredGlyphs
  | -- | R'lyeh map (recorded-set of the scenario names crossed off the map)
    RlyehMap
  | -- | Interlude III: The Awakening / Return to Arkham
    YourAlliesHaveAPlan
  | TheInvestigatorsStoodTogether
  | IsStrongInTheirFaith
  | MadeBank
  | UnderstandsTheFuture
  | FoundTheirTrueHome
  | SworeAnOathToProtectOthers
  | FoundNewWork
  | PulledTheirWeight
  | LearnedTheSecretTruth
  | LostTheirFaith
  | -- | Sepulchre of the Sleeper
    TheInvestigatorsDidNotConfrontTheNightmare
  | TheInvestigatorsHaltedCthulhusAwakening
  | {- | The Doom of Arkham. @FloodedNeighborhoods@ is a recorded-set of the card
    codes of each location that was flooded when Part I ended.
    -}
    FloodedNeighborhoods
  | CthulhuWasDrivenAway
  | CthulhuAnnihilatedTheExpedition
  | CthulhuAnnihilatedTheCityOfArkham
  | CthulhuWasBanished
  | ArkhamWasDestroyed
  deriving stock (Show, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)
