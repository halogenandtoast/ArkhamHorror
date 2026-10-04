{- | Campaign-log keys owned by Against the Wendigo.

The scenario is a side story, so these are written into whatever campaign log is
running (or the standalone one) and read back by the epilogue and by
Dr. Nadelmann's fate.
-}
module Arkham.Homebrew.AgainstTheWendigo.Key (module Arkham.Homebrew.AgainstTheWendigo.Key) where

import Arkham.CampaignLogKey (CampaignLogKey (HomebrewCampaignLogKey), IsCampaignLogKey (..))
import Arkham.Prelude

data AgainstTheWendigoKey
  = -- | Recorded by Imala Foxtail, by Charlie Foxtail's Destiny, and by damaging
    -- the Angry Sarcee Men. Turns the Angry Sarcee Men from Aloof into Hunters.
    TheSarceeAreHuntingYouDown
  | -- | Charlie Foxtail's Destiny, choice 1, healed in time
    YouSavedCharlie
  | -- | Hanninah's Gold, choice 1
    YouSavedTheGoldProspector
  | -- | Hanninah's Gold, choice 2
    YouHaveFoundHanninahsGold
  | -- | The Knowledge of the Cold, second part
    YouAreTheCustodianOfIthaquasKnowledge
  | -- | The three Students' Fate cards, read as the acts advance
    YouHaveDiscoveredBernardsFate
  | YouHaveDiscoveredNormansFate
  | YouHaveDiscoveredSylviasFate
  | -- | Act 2b: all three fates discovered
    YouHaveDiscoveredTheFateOfDrNadelmannsStudents
  | -- | Norman Falkner, survived to the end of the scenario
    NormanIsAlive
  | -- | Norman Falkner, defeated or discarded
    YouLetNormanDie
  | -- | Resolution 1
    YouDefeatedTheWendigo
  | -- | Act 3b
    YouHaveEnoughEvidenceToClearDrNadelmann
  | -- | Resolutions 2-4, when The Wendigo was still in play
    TheWendigoStillRoamsTheNorthHanninahValley
  deriving stock (Show, Read, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

instance IsCampaignLogKey AgainstTheWendigoKey where
  toCampaignLogKey = HomebrewCampaignLogKey . tshow
  fromCampaignLogKey = \case
    HomebrewCampaignLogKey t -> readMay (unpack t)
    _ -> Nothing
