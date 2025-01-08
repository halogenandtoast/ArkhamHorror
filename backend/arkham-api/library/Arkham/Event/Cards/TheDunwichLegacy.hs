module Arkham.Event.Cards.TheDunwichLegacy where

import Arkham.Asset.Uses qualified as Uses
import Arkham.Criteria qualified as Criteria
import Arkham.Event.Cards.Import

searchForTheTruth :: CardDef
searchForTheTruth =
  signature "02002"
    $ (event "02008" "Search for the Truth" 1 Neutral)
      { cdSkills = [#intellect, #intellect, #wild]
      , cdCardTraits = setFromList [Insight]
      , cdCriteria = Just $ exists $ You <> can.draw.cards FromPlayerCardEffect
      }

taunt :: CardDef
taunt =
  (event "02017" "Taunt" 1 Guardian)
    { cdCardTraits = setFromList [Tactic]
    , cdFastWindow = Just $ DuringTurn You
    , cdCriteria = Just $ exists $ CanEngageEnemy ThisCard
    , cdSkills = [#willpower, #combat]
    }

teamwork :: CardDef
teamwork =
  (event "02018" "Teamwork" 0 Guardian)
    { cdCardTraits = setFromList [Tactic]
    , cdSkills = [#wild]
    , cdCriteria =
        Just $ exists $ affectsOthers $ NotInvestigator You <> InvestigatorAt YourLocation
    }

taunt2 :: CardDef
taunt2 =
  (event "02019" "Taunt" 1 Guardian)
    { cdCardTraits = setFromList [Tactic]
    , cdFastWindow = Just $ DuringTurn You
    , cdSkills = [#willpower, #combat, #agility]
    , cdLevel = Just 2
    }

shortcut :: CardDef
shortcut =
  (event "02022" "Shortcut" 0 Seeker)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = setFromList [Insight, Tactic]
    , cdFastWindow = Just $ DuringTurn You
    , cdCriteria =
        Just
          $ exists
          $ affectsOthers
          $ can.move
          <> InvestigatorAt YourLocation
          <> InvestigatorCanMoveTo ThisCard AccessibleLocation
    }

seekingAnswers :: CardDef
seekingAnswers =
  (event "02023" "Seeking Answers" 1 Seeker)
    { cdSkills = [#intellect, #agility]
    , cdActions = [#investigate]
    , cdCardTraits = singleton Insight
    }

thinkOnYourFeet :: CardDef
thinkOnYourFeet =
  (event "02025" "Think on Your Feet" 1 Rogue)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = singleton Trick
    , cdFastWindow = Just $ EnemySpawns #when YourLocation AnyEnemy
    , cdCriteria = Just $ exists AccessibleLocation <> exists (You <> can.move)
    }

bindMonster2 :: CardDef
bindMonster2 =
  (event "02031" "Bind Monster" 3 Mystic)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = singleton Spell
    , cdActions = [#evade]
    , cdLevel = Just 2
    }

baitAndSwitch :: CardDef
baitAndSwitch =
  (event "02034" "Bait and Switch" 1 Survivor)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = setFromList [Trick]
    , cdActions = [#evade]
    }

emergencyAid :: CardDef
emergencyAid =
  (event "02105" "Emergency Aid" 2 Guardian)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = setFromList [Insight, Science]
    , cdCriteria =
        Just
          $ Criteria.AnyCriterion
            [ exists
                $ HealableAsset ThisCard #damage
                $ AssetControlledBy (affectsOthers $ InvestigatorAt YourLocation)
                <> #ally
            , exists
                $ affectsOthers
                $ HealableInvestigator ThisCard #damage
                $ InvestigatorAt YourLocation
            ]
    }

iveGotAPlan :: CardDef
iveGotAPlan =
  (event "02107" "\"I've got a plan!\"" 3 Seeker)
    { cdSkills = [#intellect, #combat]
    , cdCardTraits = setFromList [Insight, Tactic]
    , cdActions = [#fight]
    }

contraband :: CardDef
contraband =
  (event "02109" "Contraband" 4 Rogue)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = setFromList [Supply, Illicit]
    , cdCriteria =
        Just
          $ exists
          $ AssetControlledBy (affectsOthers $ InvestigatorAt YourLocation)
          <> oneOf [AssetWithUses Uses.Ammo, AssetWithUses Uses.Supply]
          <> AssetNotAtUseLimit
    }

delveTooDeep :: CardDef
delveTooDeep =
  (event "02111" "Delve Too Deep" 1 Mystic)
    { cdCardTraits = setFromList [Insight]
    , cdVictoryPoints = Just 1
    , cdCriteria =
        Just
          $ exists (You <> can.target.encounterDeck)
          <> Criteria.TabooCriteria
            TabooList15
            ( Criteria.HasCalculation
                (VictoryDisplayCountCalculation $ basic $ CardWithCardCode "02111")
                (lessThan 2)
            )
            Criteria.NoRestriction
    }

-- TODO: Oops might not be playable if 0 damage would be dealt, we might want
-- to capture that
oops :: CardDef
oops =
  (event "02113" "Oops!" 2 Survivor)
    { cdSkills = [#combat, #combat]
    , cdCardTraits = singleton Fortune
    , cdCriteria =
        Just
          $ exists (EnemyAt YourLocation <> NotEnemy AttackedEnemy <> EnemyCanBeDamagedBySource ThisCard)
          <> Criteria.CanDealDamage
    , cdFastWindow =
        Just
          $ SkillTestResult #after You (WhileAttackingAnEnemy EnemyEngagedWithYou)
          $ FailureResult
          $ lessThan 3
    , cdAlternateCardCodes = ["60518"]
    }

flare1 :: CardDef
flare1 =
  (event "02115" "Flare" 2 Survivor)
    { cdSkills = [#wild]
    , cdCardTraits = singleton Tactic
    , cdAttackOfOpportunityModifiers = [DoesNotProvokeAttacksOfOpportunity]
    , cdLevel = Just 1
    , cdCriteria = Just $ exists $ affectsOthers can.manipulate.deck
    }

standTogether3 :: CardDef
standTogether3 =
  (event "02148" "Stand Together" 0 Guardian)
    { cdSkills = [#willpower, #willpower]
    , cdCardTraits = singleton Spirit
    , cdCriteria =
        Just
          $ exists (affectsOthers $ InvestigatorAt YourLocation <> NotYou)
          <> exists
            ( affectsOthers
                $ InvestigatorAt YourLocation
                <> oneOf [can.gain.resources, can.draw.cards FromPlayerCardEffect]
            )
    , cdLevel = Just 3
    }

imOuttaHere :: CardDef
imOuttaHere =
  (event "02151" "\"I'm outta here!\"" 0 Rogue)
    { cdSkills = [#agility, #agility]
    , cdCardTraits = setFromList [Trick, Spirit]
    , cdCriteria = Just Criteria.ScenarioCardHasResignAbility
    , cdActions = [#resign]
    }

hypnoticGaze :: CardDef
hypnoticGaze =
  (event "02153" "Hypnotic Gaze" 3 Mystic)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = singleton Spell
    , cdFastWindow =
        Just
          $ EnemyAttacks
            #when
            (affectsOthers $ InvestigatorAt YourLocation)
            (CancelableEnemyAttack AnyEnemyAttack)
            AnyEnemy
    , cdAlternateCardCodes = ["60414"]
    }

lure1 :: CardDef
lure1 =
  (event "02156" "Lure" 1 Survivor)
    { cdSkills = [#agility, #agility]
    , cdCardTraits = singleton Trick
    , cdLevel = Just 1
    }

preparedForTheWorst :: CardDef
preparedForTheWorst =
  (event "02184" "Prepared for the Worst" 1 Guardian)
    { cdSkills = [#intellect, #combat]
    , cdCardTraits = singleton Tactic
    , cdCriteria = can.search.deck You
    }

preposterousSketches :: CardDef
preposterousSketches =
  (event "02186" "Preposterous Sketches" 2 Seeker)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = singleton Insight
    , cdCriteria = Just Criteria.ClueOnLocation
    , cdAlternateCardCodes = ["60218"]
    }

emergencyCache2 :: CardDef
emergencyCache2 =
  (event "02194" "Emergency Cache" 0 Neutral)
    { cdCardTraits = setFromList [Supply]
    , cdLevel = Just 2
    , cdAlternateCardCodes = ["01693"]
    , cdCriteria = Just $ exists $ You <> can.gain.resources
    }

ifItBleeds :: CardDef
ifItBleeds =
  (event "02225" "\"If it bleeds...\"" 1 Guardian)
    { cdSkills = [#willpower, #combat]
    , cdFastWindow = Just $ EnemyDefeated #after You ByAny $ EnemyWithTrait Monster
    }

exposeWeakness1 :: CardDef
exposeWeakness1 =
  (event "02228" "Expose Weakness" 0 Seeker)
    { cdSkills = [#intellect, #combat, #combat]
    , cdCardTraits = singleton Insight
    , cdFastWindow = Just FastPlayerWindow
    , cdCriteria = Just $ exists $ EnemyAt YourLocation <> EnemyWithFight
    , cdLevel = Just 1
    }

iveHadWorse4 :: CardDef
iveHadWorse4 =
  (event "02261" "\"I've had worse…\"" 0 Guardian)
    { cdSkills = [#willpower, #willpower, #agility]
    , cdCardTraits = singleton Spirit
    , cdFastWindow = Just $ DealtDamageOrHorror #when (SourceIsCancelable AnySource) You
    , cdLevel = Just 4
    , cdAlternateCardCodes = ["01684"]
    }

aceInTheHole3 :: CardDef
aceInTheHole3 =
  (event "02266" "Ace in the Hole" 0 Rogue)
    { cdCardTraits = singleton Trick
    , cdFastWindow = Just $ DuringTurn You
    , cdLevel = Just 3
    , cdExceptional = True
    }

moonlightRitual :: CardDef
moonlightRitual =
  (event "02267" "Moonlight Ritual" 0 Mystic)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = setFromList [Spell, Insight]
    , cdCriteria = Just Criteria.OwnCardWithDoom
    }

aChanceEncounter :: CardDef
aChanceEncounter =
  (event "02270" "A Chance Encounter" 1 Survivor)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = singleton Fortune
    , cdCriteria = Just $ Criteria.ReturnableCardInDiscard Criteria.AnyPlayerDiscard [Ally]
    }

momentOfRespite3 :: CardDef
momentOfRespite3 =
  (event "02273" "Moment of Respite" 3 Neutral)
    { cdSkills = [#willpower, #willpower]
    , cdCardTraits = singleton Spirit
    , cdCriteria = Just $ Criteria.Negate $ exists $ EnemyAt YourLocation
    , cdLevel = Just 3
    }

monsterSlayer5 :: CardDef
monsterSlayer5 =
  (event "02300" "Monster Slayer" 1 Guardian)
    { cdSkills = [#combat, #wild]
    , cdCardTraits = singleton Spirit
    , cdActions = [#fight]
    , cdLevel = Just 5
    }

decipheredReality5 :: CardDef
decipheredReality5 =
  (event "02303" "Deciphered Reality" 4 Seeker)
    { cdSkills = [#intellect, #intellect, #willpower]
    , cdCardTraits = singleton Insight
    , cdActions = [#investigate]
    , cdLevel = Just 5
    }

wardOfProtection5 :: CardDef
wardOfProtection5 =
  (event "02307" "Ward of Protection" 1 Mystic)
    { cdSkills = [#willpower, #wild]
    , cdCardTraits = setFromList [Spell, Spirit]
    , cdFastWindow =
        Just
          $ DrawCard
            #when
            You
            (CanCancelAllEffects $ BasicCardMatch IsEncounterCard)
            EncounterDeck
    , cdLevel = Just 5
    }
