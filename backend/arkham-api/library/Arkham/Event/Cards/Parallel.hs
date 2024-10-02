module Arkham.Event.Cards.Parallel where

import Arkham.Event.Cards.Import
import Arkham.Keyword qualified as Keyword

onTheLamAdvanced :: CardDef
onTheLamAdvanced =
  signature "90008"
    $ (event "90009" "On the Lam" 0 Neutral)
      { cdCardTraits = setFromList [Tactic]
      , cdSkills = [#intellect, #agility, #wild, #wild]
      , cdFastWindow = Just FastPlayerWindow
      , cdKeywords = singleton Keyword.Advanced
      }

darkMemoryAdvanced :: CardDef
darkMemoryAdvanced =
  (event "90019" "Dark Memory" 4 Neutral)
    { cdCardTraits = setFromList [Spell]
    , cdCardSubType = Just Weakness
    , cdLevel = Nothing
    , cdCardInHandEffects = True
    , cdKeywords = singleton Keyword.Advanced
    }

searchForTheTruthAdvanced :: CardDef
searchForTheTruthAdvanced =
  signature "90078"
    $ (event "90079" "Search for the Truth" 1 Neutral)
      { cdSkills = [#intellect, #intellect, #wild]
      , cdCardTraits = setFromList [Insight]
      , cdCriteria =
          Just
            $ exists
            $ You
            <> InvestigatorWithAnyClues
            <> oneOf
              [ can.draw.cards FromPlayerCardEffect
              , InvestigatorAt Anywhere
              ]
      , cdKeywords = singleton Keyword.Advanced
      }
