module Arkham.Event.Cards.Parallel where

import Arkham.Criteria qualified as Criteria
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
    , cdOutOfPlayEffects = [InHandEffect]
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

leadingLadyHeroine :: CardDef
leadingLadyHeroine =
  signature "90087"
    $ (event "90088" ("Leading Lady" <:> "Heroine") 1 Neutral)
      { cdSkills = [#combat, #wild]
      , cdCardTraits = setFromList [Tactic, Improvised]
      , cdKeywords = singleton Keyword.Replacement
      , cdFastWindow = Just $ DuringTurn You
      , cdCriteria = Just $ Criteria.PlayableCardExistsIgnoreModifiersFromSelf (UnpaidCost NoAction) $ InHandOf ForPlay You
      }

leadingLadyMentor :: CardDef
leadingLadyMentor =
  signature "90087"
    $ (event "90089" ("Leading Lady" <:> "Mentor") 1 Neutral)
      { cdSkills = [#intellect, #wild]
      , cdCardTraits = setFromList [Insight, Improvised]
      , cdKeywords = singleton Keyword.Replacement
      , cdFastWindow = Just $ DuringTurn You
      , cdCriteria = Just $ Criteria.PlayableCardExistsIgnoreModifiersFromSelf (UnpaidCost NoAction) $ InHandOf ForPlay You
      }

leadingLadyFemmeFatale :: CardDef
leadingLadyFemmeFatale =
  signature "90087"
    $ (event "90090" ("Leading Lady" <:> "Femme Fatale") 1 Neutral)
      { cdSkills = [#agility, #wild]
      , cdCardTraits = setFromList [Trick, Improvised]
      , cdKeywords = singleton Keyword.Replacement
      , cdFastWindow = Just $ DuringTurn You
      , cdCriteria = Just $ Criteria.PlayableCardExistsIgnoreModifiersFromSelf (UnpaidCost NoAction) $ InHandOf ForPlay You
      }

leadingLadyEnchantress :: CardDef
leadingLadyEnchantress =
  signature "90087"
    $ (event "90091" ("Leading Lady" <:> "Enchantress") 1 Neutral)
      { cdSkills = [#willpower, #wild]
      , cdCardTraits = setFromList [Ritual, Improvised]
      , cdKeywords = singleton Keyword.Replacement
      , cdFastWindow = Just $ DuringTurn You
      , cdCriteria = Just $ Criteria.PlayableCardExistsIgnoreModifiersFromSelf (UnpaidCost NoAction) $ InHandOf ForPlay You
      }

leadingLadyFinalGirl :: CardDef
leadingLadyFinalGirl =
  signature "90087"
    $ (event "90092" ("Leading Lady" <:> "Final Girl") 1 Neutral)
      { cdSkills = [#wild, #wild]
      , cdCardTraits = setFromList [Spirit, Improvised]
      , cdKeywords = singleton Keyword.Replacement
      , cdFastWindow = Just $ DuringTurn You
      , cdCriteria = Just $ Criteria.PlayableCardExistsIgnoreModifiersFromSelf (UnpaidCost NoAction) $ InHandOf ForPlay You
      }
