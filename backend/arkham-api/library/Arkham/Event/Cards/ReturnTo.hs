module Arkham.Event.Cards.ReturnTo where

import Arkham.Asset.Uses qualified as Uses
import Arkham.Criteria qualified as Criteria
import Arkham.Event.Cards.Import

dynamiteBlast2 :: CardDef
dynamiteBlast2 =
  (event "50002" "Dynamite Blast" 4 Guardian)
    { cdSkills = [#willpower, #combat]
    , cdCardTraits = setFromList [Tactic]
    , cdAttackOfOpportunityModifiers = [DoesNotProvokeAttacksOfOpportunity]
    , cdLevel = Just 2
    }

barricade3 :: CardDef
barricade3 =
  (event "50004" "Barricade" 0 Seeker)
    { cdSkills = [#willpower, #intellect, #agility]
    , cdCardTraits = setFromList [Insight, Tactic]
    , cdLevel = Just 3
    }

hotStreak2 :: CardDef
hotStreak2 =
  (event "50006" "Hot Streak" 5 Rogue)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Fortune]
    , cdLevel = Just 2
    }

mindWipe3 :: CardDef
mindWipe3 =
  (event "50008" "Mind Wipe" 1 Mystic)
    { cdSkills = [#willpower, #combat]
    , cdCardTraits = setFromList [Spell]
    , cdLevel = Just 3
    , cdFastWindow = Just $ PhaseBegins #after AnyPhase
    }

preposterousSketches2 :: CardDef
preposterousSketches2 =
  (event "51003" "Preposterous Sketches" 0 Seeker)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = singleton Insight
    , cdCriteria = Just Criteria.ClueOnLocation
    , cdLevel = Just 2
    }

contraband2 :: CardDef
contraband2 =
  (event "51005" "Contraband" 3 Rogue)
    { cdSkills = [#willpower, #intellect, #intellect]
    , cdCardTraits = setFromList [Supply, Illicit]
    , cdLevel = Just 2
    , cdCriteria =
        Just
          $ exists
          $ AssetControlledBy (affectsOthers $ InvestigatorAt YourLocation)
          <> AssetNotAtUseLimit
          <> oneOf [AssetWithUseType Uses.Ammo, AssetWithUseType Uses.Supply]
    }

thinkOnYourFeet2 :: CardDef
thinkOnYourFeet2 =
  (event "51006" "Think on Your Feet" 0 Rogue)
    { cdSkills = [#intellect, #agility, #agility]
    , cdCardTraits = singleton Trick
    , cdFastWindow = Just (EnemyEnters #when YourLocation AnyEnemy)
    , cdCriteria = Just $ exists AccessibleLocation <> exists (You <> can.move)
    , cdLevel = Just 2
    }

oops2 :: CardDef
oops2 =
  (event "51009" "Oops!" 2 Survivor)
    { cdSkills = [#combat, #combat, #agility]
    , cdCardTraits = singleton Fortune
    , cdLevel = Just 2
    , cdCriteria = Just Criteria.CanDealDamage
    , cdFastWindow =
        Just $ SkillTestResult #after You (WhileAttackingAnEnemy AnyEnemy) $ FailureResult $ lessThan 4
    }

eatLead :: CardDef
eatLead =
  (event "52002" "\"Eat lead!\"" 1 Guardian)
    { cdCardTraits = singleton Tactic
    , cdFastWindow =
        Just
          $ ActivateAbility
            #when
            You
            (AssetAbility (AssetWithTrait Firearm <> AssetWithUses Uses.Ammo) <> AbilityIsAction #fight)
    , cdSkills = [#combat, #agility]
    }

logicalReasoning4 :: CardDef
logicalReasoning4 =
  (event "52003" "Logical Reasoning" 2 Seeker)
    { cdSkills = [#willpower, #willpower, #willpower]
    , cdCardTraits = singleton Insight
    , cdLevel = Just 4
    , cdCriteria =
        Just
          $ exists (You <> InvestigatorWithAnyClues)
          <> Criteria.AnyCriterion
            [ exists $ HealableInvestigator ThisCard #horror $ InvestigatorAt YourLocation
            , exists
                $ TreacheryWithTrait Terror
                <> TreacheryInThreatAreaOf (affectsOthers $ InvestigatorAt YourLocation)
            ]
    }

stormOfSpirits3 :: CardDef
stormOfSpirits3 =
  (event "52008" "Storm of Spirits" 3 Mystic)
    { cdSkills = [#willpower, #combat, #combat]
    , cdCardTraits = singleton Spell
    , cdActions = [#fight]
    , cdLevel = Just 3
    }

bloodEclipse1 :: CardDef
bloodEclipse1 =
  (event "53001" "Blood Eclipse" 1 Guardian)
    { cdSkills = [#willpower, #combat]
    , cdCardTraits = setFromList [Spell, Spirit]
    , cdActions = [#fight]
    , cdAdditionalCost = Just $ InvestigatorDamageCost ThisCard You DamageAny 2
    , cdLevel = Just 1
    }

truthFromFiction2 :: CardDef
truthFromFiction2 =
  (event "53003" "Truth from Fiction" 1 Seeker)
    { cdSkills = [#intellect, #intellect, #intellect]
    , cdCardTraits = singleton Insight
    , cdLevel = Just 2
    , cdCriteria =
        Just
          $ exists
          $ AssetControlledBy (affectsOthers $ InvestigatorAt YourLocation)
          <> AssetCanHaveUses Uses.Secret
    }

alterFate1 :: CardDef
alterFate1 =
  (event "53009" "Alter Fate" 3 Survivor)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = setFromList [Spell, Blessed]
    , cdCriteria =
        Just $ exists $ NotTreachery (TreacheryOnEnemy EliteEnemy) <> TreacheryIsNonWeakness
    , cdLevel = Just 1
    }

trialByFire3 :: CardDef
trialByFire3 =
  (event "54010" "Trial by Fire" 2 Survivor)
    { cdSkills = [#wild, #wild]
    , cdCardTraits = singleton Spirit
    , cdFastWindow = Just $ DuringTurn You
    , cdLevel = Just 3
    }
