module Arkham.Event.Cards.Core2026 where

import Arkham.Capability
import Arkham.Criteria qualified as Criteria
import Arkham.Event.Cards.Import

detectivesIntuition :: CardDef
detectivesIntuition =
  signature "12004"
    $ (event "12005" "Detective's Intuition" 0 Seeker)
      { cdCardTraits = setFromList [Insight]
      , cdSkills = [#willpower, #intellect, #wild]
      , cdCriteria =
          Just
            $ Criteria.youExist
            $ oneOf [can.gain.resources, can.heal.damage ThisCard, can.heal.horror ThisCard]
      , cdOutOfPlayEffects = [InHandEffect]
      }

deadEnds :: CardDef
deadEnds =
  signature "12004"
    $ (event "12006" "Dead Ends" 5 Neutral)
      { cdSkills = [#wild]
      , cdCardTraits = setFromList [Blunder]
      , cdOutOfPlayEffects = [InHandEffect, InSearchEffect]
      }

lessonLearned :: CardDef
lessonLearned =
  (event "12022" "Lesson Learned" 1 Guardian)
    { cdCardTraits = setFromList [Insight, Spirit]
    , cdSkills = [#intellect, #combat]
    , cdFastWindow = Just $ DealtDamage #after (SourceIsEnemyAttack AnyEnemy) You
    , cdCriteria = Just canDiscoverCluesAtYourLocation
    }

rightToolForTheJob :: CardDef
rightToolForTheJob =
  (event "12023" "Right Tool for the Job" 1 Guardian)
    { cdCardTraits = setFromList [Insight]
    , cdSkills = [#intellect]
    , cdCriteria = Just $ Criteria.youExist can.search.deck
    }

counterattack1 :: CardDef
counterattack1 =
  (event "12026" "Counterattack" 1 Guardian)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Spirit, Tactic]
    , cdFastWindow =
        Just
          $ oneOf
            [ EnemyAttacks
                #when
                (affectsOthers $ colocatedWithMatch You)
                (CancelableEnemyAttack AnyEnemyAttack)
                AnyEnemy
            , EnemyAttacks
                #when
                (affectsOthers $ colocatedWithMatch You)
                AnyEnemyAttack
                (EnemyCanBeDamagedBySource ThisCard)
            ]
    , cdLevel = Just 1
    }
