module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Helpers where

import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Helpers.Campaign (getCampaignStoryCards)
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Treacheries qualified as HBTreacheries
import Arkham.I18n
import Arkham.Id
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Trait (Trait (DeepOne))

campaignI18n :: (HasI18n => a) -> a
campaignI18n a = withI18n $ scope "returnToTheInnsmouthConspiracy" a

scenarioI18n :: Scope -> (HasI18n => a) -> a
scenarioI18n scenarioScope a = campaignI18n $ scope scenarioScope a

{- | Read a setup line from the official scenario's own locale.

A "Return to" scenario card prints only its deltas; every instruction it leaves alone is
still the Campaign Guide's, so the setup list shows the original line rather than a copy
of it -- which keeps one string in one place and gets its translations for free. The
box's own lines sit beside them, marked with 'li.returnTo'.
-}
officialSetup :: HasI18n => Scope -> (HasI18n => a) -> a
officialSetup scenarioScope a = official scenarioScope $ scope "setup" a

{- | 'officialSetup' for anything outside the setup list, like a prompt the Return To
scenario reuses unchanged.
-}
official :: HasI18n => Scope -> (HasI18n => a) -> a
official scenarioScope a = unscoped $ scope "theInnsmouthConspiracy" $ scope scenarioScope a

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
