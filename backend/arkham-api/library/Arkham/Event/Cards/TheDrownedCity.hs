module Arkham.Event.Cards.TheDrownedCity where

import Arkham.Capability
import Arkham.Criteria qualified as Criteria
import Arkham.Event.Cards.Import

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
  (event "11023" "Primed for Action" 0 Guardian)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = setFromList [Tactic, Bold]
    , cdCriteria =
        Just
          $ Criteria.FirstAction
          <> Criteria.PlayableCardExistsWithCostReduction (Reduce 2) (InHandOf ForPlay You <> basic #firearm)
    }

readyForAnything :: CardDef
readyForAnything =
  (event "11024" "Ready for Anything" 1 Guardian)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Spirit, Bold]
    , cdCriteria = Just $ Criteria.FirstAction <> can.draw.cards You
    , cdAttackOfOpportunityModifiers = [DoesNotProvokeAttacksOfOpportunity]
    }

correlateAllItsContents :: CardDef
correlateAllItsContents =
  (event "11040" "Correlate All Its Contents" 1 Seeker)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = setFromList [Insight]
    , cdActions = [#investigate]
    }

wheresTheParty :: CardDef
wheresTheParty =
  (event "11053" "\"Where's the party?\"" 0 Rogue)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = setFromList [Trick, Improvised]
    , cdActions = [#parley]
    , cdCriteria = Just $ can.target.encounterDeck You
    }

youveHadWorse :: CardDef
youveHadWorse =
  (event "11054" "\"You've had worse…\"" 0 Rogue)
    { cdSkills = [#willpower, #agility, #agility]
    , cdCardTraits = setFromList [Favor]
    , cdFastWindow =
        Just
          $ DealtDamageOrHorror
            #when
            (SourceIsCancelable AnySource)
            (affectsOthers $ at_ (orConnected YourLocation) <> can.spend.resources)
    }

bumsRush :: CardDef
bumsRush =
  (event "11055" "Bum's Rush" 2 Rogue)
    { cdSkills = [#intellect, #combat]
    , cdCardTraits = setFromList [Tactic, Trick]
    , cdActions = [#evade]
    }
