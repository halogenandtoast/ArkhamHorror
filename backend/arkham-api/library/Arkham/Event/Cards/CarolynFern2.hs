module Arkham.Event.Cards.CarolynFern2 where

import Arkham.Event.Cards.Import

causticReaction :: CardDef
causticReaction =
  (event "60261" "Caustic Reaction" 2 Seeker)
    { cdCardTraits = setFromList [Tactic, Science]
    , cdSkills = [#combat, #intellect]
    , cdActions = #fight
    }

unflappable :: CardDef
unflappable =
  (event "60262" "Unflappable" 2 Seeker)
    { cdCardTraits = singleton Insight
    , cdSkills = [#agility, #intellect]
    , cdActions = #evade
    }

psychoanalysis :: CardDef
psychoanalysis =
  (event "60264" "Psychoanalysis" 2 Seeker)
    { cdCardTraits = setFromList [Insight, Science]
    , cdSkills = [#intellect, #wild]
    }

deEscalate :: CardDef
deEscalate =
  (event "60265" "De-Escalate" 1 Seeker)
    { cdCardTraits = singleton Insight
    , cdSkills = [#willpower, #willpower]
    , cdActions = #parley
    , cdCriteria =
        Just
          $ exists (EnemyAt YourLocation <> CanParleyEnemy You <> EnemyWithHorrorValue)
          <> exists (HealableInvestigator ThisCard #horror You)
    }

insidiousTruths :: CardDef
insidiousTruths =
  (event "60266" "Insidious Truths" 2 Seeker)
    { cdCardTraits = setFromList [Insight, Cursed]
    , cdSkills = [#agility, #combat]
    , cdActions = #fight
    , cdAdditionalCost = Just $ UpTo (Fixed 2) $ HandDiscardCost 1 #any
    }

unflappable1 :: CardDef
unflappable1 =
  (event "60271" "Unflappable" 2 Seeker)
    { cdCardTraits = singleton Insight
    , cdSkills = [#agility, #intellect, #wild]
    , cdActions = #evade
    , cdLevel = Just 1
    }

causticReaction2 :: CardDef
causticReaction2 =
  (event "60273" "Caustic Reaction" 2 Seeker)
    { cdCardTraits = setFromList [Tactic, Science]
    , cdSkills = [#combat, #combat, #intellect]
    , cdActions = #fight
    , cdLevel = Just 2
    }

hypnotize2 :: CardDef
hypnotize2 =
  (event "60274" "Hypnotize" 2 Seeker)
    { cdCardTraits = singleton Science
    , cdSkills = [#intellect, #wild, #willpower]
    , cdActions = #parley
    , cdCriteria = Just $ exists $ NonEliteEnemy <> EnemyAt YourLocation <> CanParleyEnemy You
    , cdLevel = Just 2
    }

psychoanalysis3 :: CardDef
psychoanalysis3 =
  (event "60278" "Psychoanalysis" 2 Seeker)
    { cdCardTraits = setFromList [Insight, Science]
    , cdSkills = [#intellect, #wild, #willpower]
    , cdLevel = Just 3
    }

communeWithTheCosmos5 :: CardDef
communeWithTheCosmos5 =
  (event "60282" "Commune with the Cosmos" 2 Seeker)
    { cdCardTraits = singleton Spell
    , cdSkills = [#intellect, #intellect, #wild]
    , cdActions = #investigate
    , cdLevel = Just 5
    }
