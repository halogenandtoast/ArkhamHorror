module Arkham.Event.Cards.TheDrownedCity where

import Arkham.Event.Cards.Import
import Arkham.Criteria qualified as Criteria

psychicSensitivity :: CardDef
psychicSensitivity =
  signature "11014"
    $ (event "11015" "Psychic Sensitivity" 0 Neutral)
      { cdSkills = [#willpower, #willpower, #wild]
      , cdCardTraits = setFromList [Augury, Insight]
      , cdFastWindow =
          Just
            $ DrawCard
              #when
              (at_ Anywhere)
              ( CanCancelAllEffects
                  $ basic IsEncounterCard
                  <> CardSharesTitleWith
                    (CardIsBeneathInvestigator $ InvestigatorWithTitle "Gloria Goldberg")
              )
              AnyDeck
      }

primedForAction :: CardDef
primedForAction =
    (event "11023" "Primed for Action" 0 Neutral)
      { cdSkills = [#intellect, #agility]
      , cdCardTraits = setFromList [Tactic, Bold]
      , cdCriteria = Just $ Criteria.FirstAction <> Criteria.PlayableCardExistsWithCostReduction (Reduce 2) (InHandOf ForPlay You <> basic #firearm)
      }
