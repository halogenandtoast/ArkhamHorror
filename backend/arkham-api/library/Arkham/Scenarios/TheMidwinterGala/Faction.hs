module Arkham.Scenarios.TheMidwinterGala.Faction where

import Arkham.Card
import Arkham.Helpers.Scenario (standaloneI18n)
import Arkham.I18n
import Arkham.Prelude
import Arkham.Story.Cards qualified as Stories

data Faction
  = TheFoundation
  | MiskatonicUniversity
  | TheSyndicate
  | TheSilverTwilightLodge
  | LocalsOfKingsport
  deriving stock (Show, Eq, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

factionLabel :: HasI18n => Faction -> Text
factionLabel faction = unscoped $ standaloneI18n "theMidwinterGala" $ scope "faction" $ case faction of
  TheFoundation -> "theFoundation"
  MiskatonicUniversity -> "miskatonicUniversity"
  TheSyndicate -> "theSyndicate"
  TheSilverTwilightLodge -> "theSilverTwilightLodge"
  LocalsOfKingsport -> "localsOfKingsport"

factionStoryRival :: Faction -> CardDef
factionStoryRival = \case
  TheFoundation -> Stories.theFoundationRival
  MiskatonicUniversity -> Stories.miskatonicUniversityRival
  TheSyndicate -> Stories.theSyndicateRival
  TheSilverTwilightLodge -> Stories.silverTwilightLodgeRival
  LocalsOfKingsport -> Stories.localsOfKingsportRival

factionStoryAllied :: Faction -> CardDef
factionStoryAllied = \case
  TheFoundation -> Stories.theFoundationAllied
  MiskatonicUniversity -> Stories.miskatonicUniversityAllied
  TheSyndicate -> Stories.theSyndicateAllied
  TheSilverTwilightLodge -> Stories.silverTwilightLodgeAllied
  LocalsOfKingsport -> Stories.localsOfKingsportAllied
