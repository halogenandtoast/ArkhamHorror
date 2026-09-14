module Arkham.Treachery.CardDefs.Standalone where

import Arkham.Keyword qualified as Keyword
import Arkham.Trait qualified as Trait
import Arkham.Treachery.CardDefs.Import

selfDestructive :: CardDef
selfDestructive =
  (basicWeakness "60104" "Self-Destructive") {cdCardTraits = singleton Flaw}

thriceDamnedCuriosity :: CardDef
thriceDamnedCuriosity =
  signature "60201"
    $ (weakness "60203" "Thrice-Damned Curiosity")
      { cdCardTraits = singleton Flaw
      }

lethalCuriosity :: CardDef
lethalCuriosity =
  (basicWeakness "60254" "Lethal Curiosity")
    { cdCardTraits = singleton Flaw
    }

obsessive :: CardDef
obsessive =
  (basicWeakness "60204" "Obsessive")
    { cdCardTraits = singleton Flaw
    }

darkFuture :: CardDef
darkFuture =
  signature "60401"
    $ (weakness "60403" "Dark Future")
      { cdCardTraits = setFromList [Omen, Endtimes]
      }

nihilism :: CardDef
nihilism =
  (basicWeakness "60404" "Nihilism")
    { cdCardTraits = singleton Madness
    }

calledByTheMists :: CardDef
calledByTheMists =
  signature "60501"
    $ (weakness "60503" "Called by the Mists")
      { cdCardTraits = setFromList [Curse]
      }

atychiphobia :: CardDef
atychiphobia =
  (basicWeakness "60504" "Atychiphobia")
    { cdCardTraits = setFromList [Madness]
    }

weightOfTheWorld :: CardDef
weightOfTheWorld =
  signature "60351"
    $ (weakness "60355" "Weight of the World")
      { cdCardTraits = singleton Terror
      }

realityAcid5U21 :: CardDef
realityAcid5U21 =
  signature "89001"
    $ (weakness "89004" "Reality Acid")
      { cdCardTraits = setFromList [Power]
      }

hospitalDebtsAdvanced :: CardDef
hospitalDebtsAdvanced =
  signature "01003"
    $ (weakness "90010" "Hospital Debts")
      { cdCardTraits = setFromList [Task]
      , cdKeywords = singleton Keyword.Advanced
      }

coverUpAdvanced :: CardDef
coverUpAdvanced =
  signature "01001"
    $ (weakness "90031" "Cover Up")
      { cdCardTraits = setFromList [Task]
      , cdKeywords = singleton Keyword.Advanced
      }

abandonedAndAloneAdvanced :: CardDef
abandonedAndAloneAdvanced =
  signature "01005"
    $ (weakness "90040" "Abandoned and Alone")
      { cdCardTraits = setFromList [Madness]
      , cdKeywords = singleton Keyword.Advanced
      }

hardTimes :: CardDef
hardTimes =
  signature "02005"
    $ (weakness "90048" "Hard Times")
      { cdCardTraits = setFromList [Hardship]
      , cdKeywords = singleton Keyword.Replacement
      }

finalRhapsodyAdvanced :: CardDef
finalRhapsodyAdvanced =
  signature "02004"
    $ (weakness "90051" "Final Rhapsody")
      { cdCardTraits = setFromList [Endtimes]
      , cdKeywords = singleton Keyword.Advanced
      }

smiteTheWickedAdvanced :: CardDef
smiteTheWickedAdvanced =
  signature "02001"
    $ (weakness "90061" "Smite the Wicked")
      { cdCardTraits = setFromList [Task]
      , cdKeywords = singleton Keyword.Advanced
      }

buriedSecretsAdvanced :: CardDef
buriedSecretsAdvanced =
  signature "08007"
    $ (weakness "90064" "Buried Secrets")
      { cdCardTraits = setFromList [Mystery]
      , cdKeywords = singleton Keyword.Advanced
      }

rexsCurseAdvanced :: CardDef
rexsCurseAdvanced =
  signature "02002"
    $ (weakness "90080" "Rex's Curse")
      { cdCardTraits = setFromList [Curse]
      , cdKeywords = singleton Keyword.Advanced
      }

searchingForIzzieAdvanced :: CardDef
searchingForIzzieAdvanced =
  signature "02003"
    $ (weakness "90086" "Searching for Izzie")
      { cdCardTraits = setFromList [Task]
      , cdKeywords = singleton Keyword.Advanced
      }

unaware :: CardDef
unaware =
  (basicWeakness "60356" "Unaware")
    { cdCardTraits = singleton Flaw
    }

looseCannon :: CardDef
looseCannon =
  (weakness "60153" "Loose Cannon")
    { cdCardTraits = singleton Flaw
    , cdDeckRestrictions = [Signature ("60151" :: InvestigatorId)]
    , cdLevel = Nothing
    }

overconfident :: CardDef
overconfident =
  (basicWeakness "60154" "Overconfident")
    { cdCardTraits = singleton Flaw
    }

unbrokenWeb :: CardDef
unbrokenWeb =
  (weakness "60253" "Unbroken Web")
    { cdCardTraits = setFromList [Terror, Trait.Dreamlands]
    , cdDeckRestrictions = [Signature ("60251" :: InvestigatorId)]
    , cdLevel = Nothing
    }

calledToGuinee :: CardDef
calledToGuinee =
  (weakness "60453" "Called to Guin\233e")
    { cdCardTraits = setFromList [Curse, Pact]
    , cdDeckRestrictions = [Signature ("60451" :: InvestigatorId)]
    , cdLevel = Nothing
    }

hemophobia :: CardDef
hemophobia =
  (basicWeakness "60454" "Hemophobia")
    { cdCardTraits = singleton Terror
    }

-- The Blob That Ate Everything
