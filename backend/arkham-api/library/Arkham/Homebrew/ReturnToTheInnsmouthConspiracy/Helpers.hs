module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Helpers where

import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Helpers.Campaign (getCampaignStoryCards)
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Locations qualified as HBLocations
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Treacheries qualified as HBTreacheries
import Arkham.I18n
import Arkham.Id
import Arkham.Location.CardDefs.TheInnsmouthConspiracy.FloodedCaverns qualified as Locations
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Trait (Trait (DeepOne))

campaignI18n :: (HasI18n => a) -> a
campaignI18n a = withI18n $ scope "returnToTheInnsmouthConspiracy" a

scenarioI18n :: Scope -> (HasI18n => a) -> a
scenarioI18n scenarioScope a = campaignI18n $ scope scenarioScope a

{- | "You count as a Deep One Investigator as long as you have the Deep One trait,
granted through either a scenario card or a player card. You also count as a Deep
One investigator as long as you have a permanent player card that grants the Deep
One trait added to your deck." (rules insert)

The permanent clause needs no separate test: a permanent starts the scenario in
play, so Innsmouth Influence is already granting the trait whenever it is in a
deck. Stalked by Deep Ones grants it from a threat area the same way. Both last
until the card leaves play, which is after the resolution reads -- the insert's
"persists until the beginning of the next scenario".
-}
deepOneInvestigator :: InvestigatorMatcher
deepOneInvestigator = InvestigatorWithTrait DeepOne

youAreADeepOne :: InvestigatorMatcher
youAreADeepOne = You <> deepOneInvestigator

{- | "When a scenario card instructs you to gather the Return to Flooded Caverns set,
replace one of each Tidal Pool, Underground River and Underwater Cavern from the original
Flooded Caverns set with its counterpart from the Return to Flooded Caverns. So you
should end up with six unique cards."

Both sets are gathered, which gives two copies of each of the six cards; this keeps
exactly one of each. Tidal Tunnels a scenario brings itself (Bone-Ridden Pit, The Moon
Room and the like) are left untouched.
-}
combineTidalTunnels :: [Card] -> [Card]
combineTidalTunnels cards = others <> mapMaybe one floodedCavernsPairs
 where
  others = filter ((`notElem` floodedCavernsPairs) . toCardDef) cards
  one def = find ((== def) . toCardDef) cards

floodedCavernsPairs :: [CardDef]
floodedCavernsPairs =
  [ Locations.underwaterCavern
  , HBLocations.underwaterCavern
  , Locations.tidalPool
  , HBLocations.tidalPool
  , Locations.undergroundRiver
  , HBLocations.undergroundRiver
  ]

{- | Who counts as a Deep One investigator BETWEEN scenarios. In a scenario the trait
comes from a card in play, but at campaign level Innsmouth Influence is sitting in a
deck, so the campaign's story cards are the only source of truth. 'getOwner' answers for
a single owner; every Deep One has their own copy, so all of them are needed.
-}
deepOneInvestigatorsInCampaign :: HasGame m => m [InvestigatorId]
deepOneInvestigatorsInCampaign = do
  cardMap <- getCampaignStoryCards
  pure
    [ iid
    | (iid, cards) <- mapToList cardMap
    , any ((== HBTreacheries.innsmouthInfluence) . toCardDef) cards
    ]

{- | The memory Flashback XVI records. It is not one of the campaign's printed 'Memory'
values, so it goes into the record set as its own entry -- which is the point: it counts
toward the tally that decides whether Flashback XV is read.
-}
youRememberWhereYouHaveToGo :: Text
youRememberWhereYouHaveToGo = "youRememberWhereYouHaveToGo"
