module Arkham.Event.Cards.MarieLambeau2 where

import Arkham.Criteria qualified as Criteria
import Arkham.Event.Cards.Import
import Arkham.ForMovement

consumeLife :: CardDef
consumeLife =
  (event "60462" "Consume Life" 3 Mystic)
    { cdCardTraits = setFromList [Spell]
    , cdSkills = [#combat, #willpower]
    , cdActions = CardAction #fight
    }

favorOfBaalshandor :: CardDef
favorOfBaalshandor =
  (event "60463" "Favor of Baalshandor" 0 Mystic)
    { cdCardTraits = setFromList [Ritual]
    , cdAdditionalCost = Just $ InvestigatorDamageCost ThisCard You DamageAny 1
    , cdAttackOfOpportunityModifiers = [DoesNotProvokeAttacksOfOpportunity]
    , cdCriteria =
        Just
          $ Criteria.PlayableCardExistsWithCostReduction (Reduce 3)
          $ InHandOf ForPlay You
          <> #asset
          <> basic (mapOneOf CardWithTrait [Spell, Ritual])
    }

infuseLife :: CardDef
infuseLife =
  (event "60464" "Infuse Life" 4 Mystic)
    { cdCardTraits = setFromList [Spell]
    , cdSkills = [#wild]
    , cdCriteria =
        Just
          $ oneOf
            [ exists $ HealableInvestigator ThisCard #damage $ InvestigatorAt YourLocation
            , exists $ HealableAsset ThisCard #damage $ AssetAt YourLocation <> AllyAsset
            ]
    }

mirrorForm :: CardDef
mirrorForm =
  (event "60465" "Mirror Form" 1 Mystic)
    { cdCardTraits = setFromList [Spell]
    , cdSkills = [#agility, #willpower]
    , cdFastWindow = Just (DuringTurn You)
    }

spiritualCharm :: CardDef
spiritualCharm =
  (event "60466" "Spiritual Charm" 0 Mystic)
    { cdCardTraits = setFromList [Spell, Trick]
    , cdSkills = [#agility, #willpower]
    , cdActions = CardAction #parley
    , cdCriteria =
        Just $ exists $ NonEliteEnemy <> EnemyAt (oneOf [YourLocation, ConnectedLocation NotForMovement])
    }

bloodWard2 :: CardDef
bloodWard2 =
  (event "60472" "Blood Ward" 1 Mystic)
    { cdCardTraits = setFromList [Spell]
    , cdSkills = [#combat, #wild]
    , cdLevel = Just 2
    , cdFastWindow =
        Just
          $ EnemyAttacks
            #when
            (affectsOthers $ colocatedWithMatch You)
            (CancelableEnemyAttack AnyEnemyAttack)
            NonEliteEnemy
    }

retribution2 :: CardDef
retribution2 =
  (event "60473" "Retribution" 2 Mystic)
    { cdCardTraits = setFromList [Spell, Spirit]
    , cdSkills = [#wild]
    , cdLevel = Just 2
    , cdFastWindow =
        Just
          $ EnemyAttacks
            #when
            (affectsOthers $ colocatedWithMatch You)
            AnyEnemyAttack
            AnyEnemy
    }

ultimateSacrifice4 :: CardDef
ultimateSacrifice4 =
  (event "60480" "Ultimate Sacrifice" 0 Mystic)
    { cdCardTraits = setFromList [Spell, Spirit]
    , cdSkills = [#wild]
    , cdLevel = Just 4
    , cdFastWindow = Just $ PhaseEnds #when #investigation
    }

bendBlood5 :: CardDef
bendBlood5 =
  (event "60483" "Bend Blood" 2 Mystic)
    { cdCardTraits = setFromList [Spell, Cursed]
    , cdSkills = [#combat, #combat, #willpower, #willpower]
    , cdLevel = Just 5
    , cdActions = CardAction #fight
    }
