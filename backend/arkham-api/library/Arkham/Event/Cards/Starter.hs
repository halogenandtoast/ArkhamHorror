module Arkham.Event.Cards.Starter where

import Arkham.Criteria qualified as Criteria
import Arkham.Event.Cards.Import

cleanThemOut :: CardDef
cleanThemOut =
  (event "60111" "Clean Them Out" 0 Guardian)
    { cdCardTraits = setFromList [Spirit, Tactic]
    , cdActions = [#fight]
    , cdSkills = [#willpower, #combat]
    }

counterpunch :: CardDef
counterpunch =
  (event "60112" "Counterpunch" 0 Guardian)
    { cdCardTraits = setFromList [Spirit, Tactic]
    , cdActions = [#fight]
    , cdSkills = [#combat, #agility]
    , cdFastWindow = Just $ EnemyAttacksEvenIfCancelled #after You AnyEnemyAttack AnyEnemy
    }

-- We need to override the action check for this card because of multiple actions,
-- but even if we can not fight or engage the enemy, if we can move it this should
-- still be playable
getOverHere :: CardDef
getOverHere =
  (event "60114" "\"Get over here!\"" 2 Guardian)
    { cdCardTraits = setFromList [Spirit, Tactic]
    , cdActions = [#engage, #fight]
    , cdSkills = [#willpower, #combat]
    , cdCriteria =
        Just
          $ exists
          $ NonEliteEnemy
          <> oneOf
            [ EnemyAt YourLocation <> oneOf [CanEngageEnemy ThisCard, CanFightEnemy ThisCard]
            , EnemyAt $ ConnectedFrom YourLocation
            ]
    , cdOverrideActionPlayableIfCriteriaMet = True
    }

glory :: CardDef
glory =
  (event "60115" "Glory" 1 Guardian)
    { cdCardTraits = singleton Spirit
    , cdSkills = [#intellect, #intellect]
    , cdFastWindow = Just $ EnemyDefeated #after You ByAny AnyEnemy
    }

monsterSlayer :: CardDef
monsterSlayer =
  (event "60116" "Monster Slayer" 0 Guardian)
    { cdCardTraits = singleton Spirit
    , cdActions = [#fight]
    , cdSkills = [#wild]
    }

oneTwoPunch :: CardDef
oneTwoPunch =
  (event "60117" "One-Two Punch" 2 Guardian)
    { cdCardTraits = setFromList [Spirit, Tactic]
    , cdActions = [#fight]
    , cdSkills = [#combat]
    }

standTogether :: CardDef
standTogether =
  (event "60118" "Stand Together" 0 Guardian)
    { cdCardTraits = singleton Spirit
    , cdSkills = [#willpower]
    , cdCriteria =
        Just
          $ exists (affectsOthers $ NotYou <> InvestigatorAt YourLocation)
          <> exists (affectsOthers $ InvestigatorAt YourLocation <> can.gain.resources)
    }

evidence1 :: CardDef
evidence1 =
  (event "60120" "Evidence!" 1 Guardian)
    { cdSkills = [#intellect, #intellect]
    , cdCardTraits = singleton Insight
    , cdFastWindow = Just (EnemyDefeated #after You ByAny AnyEnemy)
    , cdCriteria = Just canDiscoverCluesAtYourLocation
    , cdLevel = Just 1
    }

galvanize1 :: CardDef
galvanize1 =
  (event "60121" "Galvanize" 2 Guardian)
    { cdSkills = [#willpower, #willpower]
    , cdCardTraits = singleton Spirit
    , cdFastWindow = Just $ DuringTurn You
    , cdLevel = Just 1
    }

counterpunch2 :: CardDef
counterpunch2 =
  (event "60122" "Counterpunch" 0 Guardian)
    { cdSkills = [#combat, #combat, #agility]
    , cdCardTraits = setFromList [Spirit, Tactic]
    , cdFastWindow = Just $ EnemyAttacks #when You AnyEnemyAttack AnyEnemy
    , cdLevel = Just 2
    }

-- We need to override the action check for this card because of multiple actions,
-- but even if we can not fight or engage the enemy, if we can move it this should
-- still be playable
getOverHere2 :: CardDef
getOverHere2 =
  (event "60123" "\"Get over here!\"" 2 Guardian)
    { cdCardTraits = setFromList [Spirit, Tactic]
    , cdActions = [#engage, #fight]
    , cdSkills = [#willpower, #willpower, #combat]
    , cdFastWindow = Just FastPlayerWindow
    , cdCriteria =
        Just
          $ exists
          $ NonEliteEnemy
          <> oneOf
            [ EnemyAt YourLocation <> oneOf [CanEngageEnemy ThisCard, CanFightEnemy ThisCard]
            , EnemyAt $ ConnectedFrom YourLocation
            , EnemyAt $ LocationWithDistanceFrom 2 YourLocation Anywhere
            ]
    , cdOverrideActionPlayableIfCriteriaMet = True
    , cdLevel = Just 2
    }

lessonLearned2 :: CardDef
lessonLearned2 =
  (event "60124" "Lesson Learned" 1 Guardian)
    { cdCardTraits = setFromList [Insight, Spirit]
    , cdSkills = [#willpower, #intellect, #intellect]
    , cdFastWindow = Just $ DealtDamage #after (SourceIsEnemyAttack AnyEnemy) You
    , cdCriteria = Just canDiscoverCluesAtYourLocation
    , cdLevel = Just 2
    }

manoAMano2 :: CardDef
manoAMano2 =
  (event "60125" "Mano a Mano" 0 Guardian)
    { cdSkills = [#willpower, #combat]
    , cdCardTraits = setFromList [Spirit, Bold]
    , cdCriteria =
        Just
          $ Criteria.FirstAction
          <> exists (EnemyEngagedWithYou <> EnemyCanBeDamagedBySource ThisCard)
          <> Criteria.CanDealDamage
    , cdAttackOfOpportunityModifiers = [DoesNotProvokeAttacksOfOpportunity]
    , cdLevel = Just 2
    }

dynamiteBlast3 :: CardDef
dynamiteBlast3 =
  (event "60129" "Dynamite Blast" 4 Guardian)
    { cdSkills = [#willpower, #willpower, #combat, #combat]
    , cdCardTraits = setFromList [Tactic]
    , cdLevel = Just 3
    }

taunt3 :: CardDef
taunt3 =
  (event "60130" "Taunt" 1 Guardian)
    { cdCardTraits = setFromList [Tactic]
    , cdFastWindow = Just $ DuringTurn You
    , cdSkills = [#willpower, #willpower, #combat, #agility]
    , cdLevel = Just 3
    }

oneTwoPunch5 :: CardDef
oneTwoPunch5 =
  (event "60132" "One-Two Punch" 2 Guardian)
    { cdCardTraits = setFromList [Spirit, Tactic]
    , cdActions = [#fight]
    , cdSkills = [#combat, #combat, #combat, #combat]
    , cdLevel = Just 5
    }

burningTheMidnightOil :: CardDef
burningTheMidnightOil =
  (event "60214" "Burning the Midnight Oil" 0 Seeker)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = singleton Insight
    , cdActions = [#investigate]
    }

crypticWritings :: CardDef
crypticWritings =
  (event "60215" "Cryptic Writings" 0 Seeker)
    { cdSkills = [#intellect, #intellect]
    , cdCardTraits = singleton Insight
    , cdCardInHandEffects = True
    , cdCriteria = Just $ can.gain.resources You
    }

extensiveResearch :: CardDef
extensiveResearch =
  (event "60216" "Extensive Research" 12 Seeker)
    { cdSkills = [#intellect, #intellect]
    , cdCardTraits = singleton Insight
    , cdCardInHandEffects = True
    , cdCriteria = Just canDiscoverCluesAtYourLocation
    }

occultInvocation :: CardDef
occultInvocation =
  (event "60217" "Occult Invocation" 2 Seeker)
    { cdSkills = [#combat, #intellect]
    , cdCardTraits = singleton Spell
    , cdAdditionalCost = Just $ UpTo (Fixed 2) $ HandDiscardCost 1 #any
    , cdActions = [#fight]
    }

glimpseTheUnthinkable1 :: CardDef
glimpseTheUnthinkable1 =
  (event "60221" "Glimpse the Unthinkable" 0 Seeker)
    { cdSkills = [#intellect, #intellect]
    , cdCardTraits = singleton Insight
    , cdLevel = Just 1
    , cdCriteria = Just $ Criteria.AnyCriterion [Criteria.CanDrawCards, Criteria.CanManipulateDeck]
    }

crypticWritings2 :: CardDef
crypticWritings2 =
  (event "60224" "Cryptic Writings" 0 Seeker)
    { cdSkills = [#intellect, #wild]
    , cdCardTraits = singleton Insight
    , cdCardInHandEffects = True
    , cdLevel = Just 2
    , cdCriteria = Just $ can.gain.resources You
    }

iveGotAPlan2 :: CardDef
iveGotAPlan2 =
  (event "60225" "\"I've got a plan!\"" 2 Seeker)
    { cdSkills = [#intellect, #intellect, #combat]
    , cdCardTraits = setFromList [Insight, Tactic]
    , cdLevel = Just 2
    , cdActions = [#fight]
    }

mindOverMatter2 :: CardDef
mindOverMatter2 =
  (event "60226" "Mind over Matter" 1 Seeker)
    { cdSkills = [#combat, #agility, #wild]
    , cdCardTraits = singleton Insight
    , cdFastWindow = Just $ DuringTurn You
    , cdLevel = Just 2
    }

seekingAnswers2 :: CardDef
seekingAnswers2 =
  (event "60227" "Seeking Answers" 1 Seeker)
    { cdSkills = [#intellect, #agility, #agility]
    , cdActions = [#investigate]
    , cdCardTraits = singleton Insight
    , cdLevel = Just 2
    , cdAlternateCardCodes = ["01685"]
    }

pilfer :: CardDef
pilfer =
  (event "60315" "Pilfer" 4 Rogue)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = singleton Trick
    , cdActions = [#investigate]
    }

sneakBy :: CardDef
sneakBy =
  (event "60316" "Sneak By" 0 Rogue)
    { cdCardTraits = singleton Trick
    , cdActions = [#evade]
    , cdSkills = [#agility, #agility]
    }

daringManeuver2 :: CardDef
daringManeuver2 =
  (event "60322" "Daring Maneuver" 0 Rogue)
    { cdSkills = [#wild]
    , cdCardTraits = singleton Gambit
    , cdFastWindow = Just $ WouldHaveSkillTestResult #when You AnySkillTest $ SuccessResult AnyValue
    , cdLevel = Just 2
    }

cheapShot2 :: CardDef
cheapShot2 =
  (event "60323" "Cheap Shot" 2 Rogue)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = singleton Trick
    , cdActions = [#fight]
    , cdLevel = Just 2
    }

slipAway2 :: CardDef
slipAway2 =
  (event "60324" "Slip Away" 2 Rogue)
    { cdCardTraits = singleton Trick
    , cdSkills = [#intellect, #agility]
    , cdActions = [#evade]
    , cdLevel = Just 2
    }

pilfer3 :: CardDef
pilfer3 =
  (event "60328" "Pilfer" 4 Rogue)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = singleton Trick
    , cdActions = [#investigate]
    , cdLevel = Just 3
    }

backstab3 :: CardDef
backstab3 =
  (event "60329" "Backstab" 3 Rogue)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Tactic]
    , cdActions = [#fight]
    , cdLevel = Just 3
    }

parallelFates :: CardDef
parallelFates =
  (event "60415" "Parallel Fates" 0 Mystic)
    { cdSkills = [#wild]
    , cdCardTraits = singleton Augury
    , cdCriteria = Just $ exists $ You <> can.target.encounterDeck
    }

voiceOfRa :: CardDef
voiceOfRa =
  (event "60416" "Voice of Ra" 0 Mystic)
    { cdSkills = [#willpower]
    , cdCardTraits = singleton Spell
    , cdCriteria = Just $ exists $ can.gain.resources <> You
    }

eldritchInspiration1 :: CardDef
eldritchInspiration1 =
  (event "60420" "Eldritch Inspiration" 0 Mystic)
    { cdSkills = [#willpower, #intellect, #intellect]
    , cdCardTraits = setFromList [Spell, Spirit]
    , cdFastWindow = Just $ WouldTriggerChaosTokenRevealEffectOnCard You MysticCard [minBound ..]
    , cdLevel = Just 1
    }

hypnoticGaze2 :: CardDef
hypnoticGaze2 =
  (event "60423" "Hypnotic Gaze" 2 Mystic)
    { cdSkills = [#combat, #agility, #agility]
    , cdCardTraits = singleton Spell
    , cdFastWindow =
        Just
          $ EnemyAttacks
            #when
            (affectsOthers $ InvestigatorAt YourLocation)
            (CancelableEnemyAttack AnyEnemyAttack)
            AnyEnemy
    , cdLevel = Just 2
    }

recharge4 :: CardDef
recharge4 =
  (event "60429" "Recharge" 0 Mystic)
    { cdSkills = [#willpower, #willpower, #willpower]
    , cdCardTraits = singleton Spell
    , cdCriteria =
        Just
          $ exists
          $ AssetControlledBy (affectsOthers $ InvestigatorAt YourLocation)
          <> oneOf [AssetWithTrait Spell, AssetWithTrait Relic]
    , cdLevel = Just 4
    }

willToSurvive :: CardDef
willToSurvive =
  (event "60512" "Will to Survive" 4 Survivor)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Spirit]
    , cdFastWindow = Just (DuringTurn You)
    }

aTestOfWill :: CardDef
aTestOfWill =
  (event "60513" "A Test of Will" 1 Survivor)
    { cdSkills = [#willpower]
    , cdCardTraits = singleton Spirit
    , cdFastWindow =
        Just
          $ OrWindowMatcher
            [ DrawCard
                #when
                (InvestigatorAt YourLocation)
                (basic $ NonPeril <> NonWeaknessTreachery)
                EncounterDeck
            , DrawCard #when You (basic NonWeaknessTreachery) EncounterDeck
            ]
    }

gritYourTeeth :: CardDef
gritYourTeeth =
  (event "60515" "Grit Your Teeth" 1 Survivor)
    { cdSkills = [#wild]
    , cdCardTraits = singleton Spirit
    , cdFastWindow = Just $ SkillTestResult #after You AnySkillTest $ FailureResult AnyValue
    }

aTestOfWill2 :: CardDef
aTestOfWill2 =
  (event "60523" "A Test of Will" 0 Survivor)
    { cdSkills = [#willpower]
    , cdCardTraits = singleton Spirit
    , cdFastWindow =
        Just
          $ OrWindowMatcher
            [ DrawCard
                #when
                (affectsOthers $ InvestigatorAt YourLocation)
                (CanCancelRevelationEffect $ basic $ NonPeril <> NonWeaknessTreachery)
                EncounterDeck
            , DrawCard #when You (CanCancelRevelationEffect $ basic NonWeaknessTreachery) EncounterDeck
            ]
    , cdLevel = Just 2
    }

lookWhatIFound2 :: CardDef
lookWhatIFound2 =
  (event "60524" "\"Look what I found!\"" 2 Survivor)
    { cdSkills = [#intellect, #intellect, #agility]
    , cdCardTraits = singleton Fortune
    , cdLevel = Just 2
    , cdCriteria =
        Just
          $ Criteria.Criteria
            [ exists $ LocationMatchAny [YourLocation, ConnectedLocation] <> LocationWithAnyClues
            , exists
                $ You
                <> InvestigatorCanDiscoverCluesAt (LocationMatchAny [YourLocation, ConnectedLocation])
            ]
    , cdFastWindow =
        Just $ SkillTestResult #after You (WhileInvestigating Anywhere) $ FailureResult $ lessThan 4
    }

dumbLuck2 :: CardDef
dumbLuck2 =
  (event "60525" "Dumb Luck" 2 Survivor)
    { cdSkills = [#willpower, #agility, #agility]
    , cdCardTraits = singleton Fortune
    , cdFastWindow =
        Just $ SkillTestResult #after You (WhileEvadingAnEnemy NonEliteEnemy) $ FailureResult $ lessThan 4
    , cdLevel = Just 2
    , cdCriteria = Just $ exists $ You <> can.target.encounterDeck
    }

lucky3 :: CardDef
lucky3 =
  (event "60528" "Lucky!" 0 Survivor)
    { cdCardTraits = singleton Fortune
    , cdFastWindow =
        Just
          $ WouldHaveSkillTestResult #when (affectsOthers $ InvestigatorAt YourLocation) AnySkillTest
          $ FailureResult AnyValue
    , cdLevel = Just 3
    }
