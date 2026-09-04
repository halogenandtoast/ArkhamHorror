module Arkham.Treachery.CardDefs.ReturnTo where

import Arkham.Keyword qualified as Keyword
import Arkham.Treachery.CardDefs.Import

throughTheGates :: CardDef
throughTheGates =
  (basicWeakness "51011" "Through the Gates") {cdCardTraits = setFromList [Pact, Mystery]}

unspeakableOathBloodthirst :: CardDef
unspeakableOathBloodthirst =
  (basicWeakness "52011" ("Unspeakable Oath" <:> "Bloodthirst"))
    { cdCardTraits = setFromList [Madness, Pact]
    , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden]
    , cdDeckRestrictions = [CampaignModeOnly]
    }

unspeakableOathCuriosity :: CardDef
unspeakableOathCuriosity =
  (basicWeakness "52012" ("Unspeakable Oath" <:> "Curiosity"))
    { cdCardTraits = setFromList [Madness, Pact]
    , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden]
    , cdDeckRestrictions = [CampaignModeOnly]
    }

unspeakableOathCowardice :: CardDef
unspeakableOathCowardice =
  (basicWeakness "52013" ("Unspeakable Oath" <:> "Cowardice"))
    { cdCardTraits = setFromList [Madness, Pact]
    , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden]
    , cdDeckRestrictions = [CampaignModeOnly]
    }

offerYouCannotRefuse :: CardDef
offerYouCannotRefuse =
  (basicWeakness "53013" "Offer You Cannot Refuse")
    { cdCardTraits = singleton Pact
    , cdDeckRestrictions = [CampaignModeOnly]
    , cdGrantedXp = Just 2
    }

finePrint :: CardDef
finePrint = (weakness "53014" "Fine Print") {cdCardTraits = singleton Pact}

sellYourSoul :: CardDef
sellYourSoul = (weakness "53015" "Sell Your Soul") {cdCardTraits = singleton Pact}

damned :: CardDef
damned =
  (basicWeakness "54014" "Damned")
    { cdCardTraits = setFromList [Curse, Omen]
    , cdPermanent = True
    , cdTags = [setupOnlyTag]
    }
