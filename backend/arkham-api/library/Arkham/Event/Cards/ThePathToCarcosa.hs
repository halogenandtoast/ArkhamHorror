module Arkham.Event.Cards.ThePathToCarcosa where

import Arkham.Asset.Uses qualified as Uses
import Arkham.Criteria qualified as Criteria
import Arkham.Event.Cards.Import

thePaintedWorld :: CardDef
thePaintedWorld =
  signature "03003"
    $ (event "03012" "The Painted World" 0 Neutral)
      { cdSkills = [#willpower, #agility, #wild]
      , cdCardTraits = singleton Spell
      , cdSkipPlayWindows = True
      , cdFastWindow =
          Just
            $ PlayerHasPlayableCard (UnpaidCost NeedsAction)
            $ CardIsBeneathInvestigator You
            <> basic (NonExceptional <> #event)
      , cdCost = Nothing
      }

buryThemDeep :: CardDef
buryThemDeep =
  signature "03005"
    $ (event "03016" "Bury Them Deep" 0 Neutral)
      { cdSkills = [#willpower, #combat, #wild]
      , cdCardTraits = singleton Task
      , cdFastWindow = Just $ EnemyDefeated #after Anyone ByAny $ NonEliteEnemy <> EnemyAt YourLocation
      , cdVictoryPoints = Just 1
      }

improvisation :: CardDef
improvisation =
  signature "03006"
    $ (event "03018" "Improvisation" 0 Neutral)
      { cdSkills = [#wild, #wild]
      , cdCardTraits = singleton Insight
      , cdFastWindow = Just $ DuringTurn You
      }

letMeHandleThis :: CardDef
letMeHandleThis =
  (event "03022" "\"Let me handle this!\"" 0 Guardian)
    { cdSkills = [#willpower, #combat]
    , cdCardTraits = setFromList [Spirit]
    , cdFastWindow =
        Just $ DrawCard #after (affectsOthers NotYou) (basic $ NonPeril <> IsEncounterCard) AnyDeck
    }

everVigilant1 :: CardDef
everVigilant1 =
  (event "03023" "Ever Vigilant" 0 Guardian)
    { cdSkills = [#intellect, #intellect]
    , cdCardTraits = singleton Tactic
    , cdLevel = Just 1
    , cdCriteria = Just $ Criteria.PlayableCardExistsWithCostReduction (Reduce 1) $ #asset <> InHandOf You
    }

noStoneUnturned :: CardDef
noStoneUnturned =
  (event "03026" "No Stone Unturned" 2 Seeker)
    { cdSkills = [#wild]
    , cdCardTraits = singleton Insight
    , cdCriteria =
        Just $ exists $ affectsOthers $ InvestigatorAt YourLocation <> can.manipulate.deck
    }

sleightOfHand :: CardDef
sleightOfHand =
  (event "03029" "Sleight of Hand" 1 Rogue)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = singleton Trick
    , cdFastWindow = Just $ DuringTurn You
    , cdCriteria = Just $ Criteria.PlayableCardExists PaidCost $ InHandOf You <> #item
    }

daringManeuver :: CardDef
daringManeuver =
  (event "03030" "Daring Maneuver" 0 Rogue)
    { cdSkills = [#wild]
    , cdCardTraits = singleton Gambit
    , cdFastWindow = Just $ WouldHaveSkillTestResult #when You AnySkillTest $ SuccessResult AnyValue
    , cdAlternateCardCodes = ["60313"]
    }

uncageTheSoul :: CardDef
uncageTheSoul =
  (event "03033" "Uncage the Soul" 0 Mystic)
    { cdSkills = [#willpower, #willpower]
    , cdCardTraits = singleton Spirit
    , cdCriteria =
        Just
          $ Criteria.PlayableCardExistsWithCostReduction (Reduce 3)
          $ InHandOf You
          <> basic (oneOf [CardWithTrait Spell, CardWithTrait Ritual])
    }

astralTravel :: CardDef
astralTravel =
  (event "03034" "Astral Travel" 3 Mystic)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = singleton Spell
    , cdActions = [#move]
    , cdCriteria = Just $ exists $ RevealedLocation <> Unblocked <> NotYourLocation
    , cdAlternateCardCodes = ["60413"]
    }

hidingSpot :: CardDef
hidingSpot =
  (event "03038" "Hiding Spot" 1 Survivor)
    { cdSkills = [#agility, #agility]
    , cdCardTraits = setFromList [Tactic, Trick]
    , cdFastWindow = Just FastPlayerWindow
    }

heroicRescue :: CardDef
heroicRescue =
  (event "03106" "Heroic Rescue" 1 Guardian)
    { cdSkills = [#willpower, #combat]
    , cdCardTraits = setFromList [Spirit, Tactic]
    , cdFastWindow =
        Just
          $ EnemyWouldAttack
            #when
            (affectsOthers $ NotYou <> InvestigatorAt YourLocation)
            AnyEnemyAttack
            NonEliteEnemy
    }

anatomicalDiagrams :: CardDef
anatomicalDiagrams =
  (event "03108" "Anatomical Diagrams" 1 Seeker)
    { cdSkills = [#willpower, #combat]
    , cdCardTraits = singleton Insight
    , cdFastWindow = Just $ DuringTurn Anyone
    , cdCriteria =
        Just
          $ exists (You <> InvestigatorWithRemainingSanity (atLeast 5))
          <> exists (EnemyAt YourLocation <> NonEliteEnemy)
    }

ambush1 :: CardDef
ambush1 =
  (event "03148" "Ambush" 2 Guardian)
    { cdSkills = [#intellect, #combat]
    , cdCardTraits = singleton Tactic
    , cdLevel = Just 1
    }

forewarned1 :: CardDef
forewarned1 =
  (event "03150" "Forewarned" 0 Seeker)
    { cdSkills = [#willpower]
    , cdCardTraits = singleton Insight
    , cdLevel = Just 1
    , cdCriteria = Just $ exists (You <> oneOf [InvestigatorWithAnyClues, canParallelRexClues])
    , cdFastWindow =
        Just $ DrawCard #when You (CanCancelRevelationEffect $ basic NonWeaknessTreachery) EncounterDeck
    }

sneakAttack2 :: CardDef
sneakAttack2 =
  (event "03152" "Sneak Attack" 2 Rogue)
    { cdSkills = [#intellect, #combat, #combat]
    , cdCardTraits = setFromList [Tactic]
    , cdLevel = Just 2
    , cdCriteria =
        Just $ exists (EnemyAt YourLocation <> EnemyNotEngagedWithYou) <> Criteria.CanDealDamage
    }

stormOfSpirits :: CardDef
stormOfSpirits =
  (event "03153" "Storm of Spirits" 3 Mystic)
    { cdSkills = [#willpower, #combat]
    , cdCardTraits = singleton Spell
    , cdActions = [#fight]
    }

fightOrFlight :: CardDef
fightOrFlight =
  (event "03155" "Fight or Flight" 1 Survivor)
    { cdCardTraits = singleton Spirit
    , cdFastWindow = Just $ DuringTurn You
    }

aTestOfWill1 :: CardDef
aTestOfWill1 =
  (event "03156" "A Test of Will" 1 Survivor)
    { cdSkills = [#willpower]
    , cdCardTraits = singleton Spirit
    , cdFastWindow =
        Just
          $ OrWindowMatcher
            [ DrawCard
                #when
                (InvestigatorAt YourLocation)
                (CanCancelRevelationEffect $ basic $ NonPeril <> NonWeaknessTreachery)
                EncounterDeck
            , DrawCard
                #when
                You
                (CanCancelRevelationEffect $ basic NonWeaknessTreachery)
                EncounterDeck
            ]
    , cdLevel = Just 1
    }

devilsLuck :: CardDef
devilsLuck =
  (event "03157" "Devil's Luck" 1 Survivor)
    { cdSkills = [#agility]
    , cdCardTraits = singleton Fortune
    , cdFastWindow = Just $ DealtDamageOrHorror #when (SourceIsCancelable AnySource) You
    , cdLevel = Just 1
    }

callingInFavors :: CardDef
callingInFavors =
  (event "03158" "Calling in Favors" 1 Neutral)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = singleton Favor
    , cdCriteria = Just $ exists $ AssetControlledBy You
    }

illSeeYouInHell :: CardDef
illSeeYouInHell =
  (event "03189" "\"I'll see you in hell!\"" 0 Guardian)
    { cdSkills = [#combat, #combat]
    , cdCardTraits = singleton Spirit
    , cdAttackOfOpportunityModifiers = [DoesNotProvokeAttacksOfOpportunity]
    }

logicalReasoning :: CardDef
logicalReasoning =
  (event "03191" "Logical Reasoning" 2 Seeker)
    { cdSkills = [#willpower, #willpower]
    , cdCardTraits = singleton Insight
    , cdCriteria =
        Just
          $ exists (You <> InvestigatorWithAnyClues)
          <> Criteria.AnyCriterion
            [ exists
                $ HealableInvestigator ThisCard #horror
                $ InvestigatorAt YourLocation
            , exists
                $ TreacheryWithTrait Terror
                <> TreacheryInThreatAreaOf (affectsOthers $ InvestigatorAt YourLocation)
            ]
    }

cheapShot :: CardDef
cheapShot =
  (event "03194" "Cheap Shot" 2 Rogue)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Trick]
    , cdActions = [#fight]
    , cdAlternateCardCodes = ["60312"]
    }

quantumFlux :: CardDef
quantumFlux =
  (event "03196" "Quantum Flux" 1 Mystic)
    { cdSkills = [#wild]
    , cdCardTraits = singleton Insight
    }

recharge2 :: CardDef
recharge2 =
  (event "03197" "Recharge" 0 Mystic)
    { cdSkills = [#willpower, #willpower]
    , cdCardTraits = singleton Spell
    , cdCriteria =
        Just
          $ exists
          $ AssetControlledBy (affectsOthers $ InvestigatorAt YourLocation)
          <> oneOf [AssetWithTrait Spell, AssetWithTrait Relic]
    , cdLevel = Just 2
    }

snareTrap2 :: CardDef
snareTrap2 =
  (event "03199" "Snare Trap" 2 Survivor)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = setFromList [Trap, Improvised]
    , cdCriteria = Just $ Criteria.Negate $ exists $ "Snare Trap" <> AssetAt YourLocation
    , cdLevel = Just 2
    }

manoAMano1 :: CardDef
manoAMano1 =
  (event "03229" "Mano a Mano" 0 Guardian)
    { cdSkills = [#willpower, #combat]
    , cdCardTraits = setFromList [Spirit, Bold]
    , cdCriteria =
        Just $ Criteria.FirstAction <> exists EnemyEngagedWithYou <> Criteria.CanDealDamage
    , cdAttackOfOpportunityModifiers = [DoesNotProvokeAttacksOfOpportunity]
    , cdLevel = Just 1
    }

shortcut2 :: CardDef
shortcut2 =
  (event "03232" "Shortcut" 1 Seeker)
    { cdSkills = [#willpower, #intellect, #agility]
    , cdCardTraits = setFromList [Insight, Tactic]
    , cdFastWindow = Just $ DuringTurn You
    , cdLevel = Just 2
    }

waylay :: CardDef
waylay =
  (event "03237" "Waylay" 3 Survivor)
    { cdSkills = [#agility, #agility]
    , cdCardTraits = singleton Tactic
    , cdCriteria =
        Just $ exists $ NonEliteEnemy <> EnemyAt YourLocation <> ExhaustedEnemy <> EnemyWithEvade
    }

aChanceEncounter2 :: CardDef
aChanceEncounter2 =
  (event "03238" "A Chance Encounter" 0 Survivor)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = singleton Fortune
    , cdCost = Just DynamicCost
    , cdCriteria = Just $ Criteria.ReturnableCardInDiscard Criteria.AnyPlayerDiscard [Ally]
    , cdLevel = Just 2
    }

emergencyCache3 :: CardDef
emergencyCache3 =
  (event "03239" "Emergency Cache" 0 Neutral)
    { cdCardTraits = setFromList [Supply]
    , cdLevel = Just 3
    , cdCriteria =
        Just
          $ Criteria.AnyCriterion
            [ exists $ You <> can.gain.resources
            , exists
                $ AssetWithUseType Uses.Supply
                <> AssetControlledBy (affectsOthers $ InvestigatorAt YourLocation)
                <> AssetNotAtUseLimit
            ]
    }

onTheHunt :: CardDef
onTheHunt =
  (event "03263" "On the Hunt" 1 Guardian)
    { cdCardTraits = singleton Tactic
    , cdFastWindow = Just $ WouldDrawEncounterCard #when You #mythos
    , cdSkills = [#intellect, #combat]
    }

guidance :: CardDef
guidance =
  (event "03265" "Guidance" 0 Seeker)
    { cdCardTraits = singleton Insight
    , cdCriteria =
        Just $ exists $ affectsOthers $ NotYou <> InvestigatorAt YourLocation <> YetToTakeTurn
    , cdSkills = [#wild]
    }

narrowEscape :: CardDef
narrowEscape =
  (event "03267" "Narrow Escape" 0 Rogue)
    { cdCardTraits = singleton Fortune
    , cdSkills = [#agility, #agility]
    , cdFastWindow =
        Just
          $ EnemyAttacks
            #when
            You
            (CancelableEnemyAttack AttackOfOpportunityAttack)
            AnyEnemy
    }

wardOfProtection2 :: CardDef
wardOfProtection2 =
  (event "03270" "Ward of Protection" 1 Mystic)
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Spell, Spirit]
    , cdFastWindow =
        Just
          $ oneOf
            [ DrawCard
                #when
                (affectsOthers $ InvestigatorAt YourLocation)
                (CanCancelRevelationEffect $ basic $ NonPeril <> NonWeaknessTreachery)
                EncounterDeck
            , DrawCard
                #when
                You
                (CanCancelRevelationEffect $ basic $ NonWeaknessTreachery)
                EncounterDeck
            ]
    , cdLevel = Just 2
    }

trueSurvivor3 :: CardDef
trueSurvivor3 =
  (event "03273" "True Survivor" 3 Survivor)
    { cdCardTraits = singleton Spirit
    , cdCriteria = Just $ Criteria.CardInDiscard (Criteria.DiscardOf You) (CardWithTrait Innate)
    , cdLevel = Just 3
    }

eatLead2 :: CardDef
eatLead2 =
  (event "03304" "\"Eat lead!\"" 0 Guardian)
    { cdCardTraits = singleton Tactic
    , cdFastWindow =
        Just
          $ ActivateAbility
            #when
            You
            (AssetAbility (AssetWithTrait Firearm <> AssetWithUses Uses.Ammo) <> AbilityIsAction #fight)
    , cdLevel = Just 2
    , cdSkills = [#combat, #agility]
    }

eideticMemory3 :: CardDef
eideticMemory3 =
  (event "03306" "Eidetic Memory" 0 Seeker)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = singleton Spirit
    , cdFastWindow =
        Just
          $ PlayerHasPlayableCard (UnpaidCost NeedsAction)
          $ InDiscardOf Anyone
          <> basic (CardWithTrait Insight <> #event)
    , cdLevel = Just 3
    , cdCost = Nothing
    }

noStoneUnturned5 :: CardDef
noStoneUnturned5 =
  (event "03307" "No Stone Unturned" 2 Seeker)
    { cdCardTraits = singleton Insight
    , cdSkills = [#wild, #intellect]
    , cdFastWindow = Just FastPlayerWindow
    , cdCriteria =
        Just $ exists $ affectsOthers $ InvestigatorAt YourLocation <> can.manipulate.deck
    , cdLevel = Just 5
    }

cheatDeath5 :: CardDef
cheatDeath5 =
  (event "03310" "Cheat Death" 1 Rogue)
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Trick, Fated]
    , cdFastWindow = Just $ InvestigatorWouldBeDefeated #when ByAny You
    , cdLevel = Just 5
    }

timeWarp2 :: CardDef
timeWarp2 =
  (event "03311" "Time Warp" 1 Mystic)
    { cdCardTraits = setFromList [Spell, Paradox]
    , cdFastWindow =
        Just $ PerformAction #after (affectsOthers $ InvestigatorAt YourLocation) AnyAction
    , cdCriteria = Just $ Criteria.ActionCanBeUndone <> Criteria.DuringTurn Anyone
    , cdLevel = Just 2
    }

infighting3 :: CardDef
infighting3 =
  (event "03314" "Infighting" 1 Survivor)
    { cdSkills = [#intellect, #intellect, #agility, #agility]
    , cdCardTraits = singleton Trick
    , cdLevel = Just 3
    , cdFastWindow = Just $ PhaseBegins #after #enemy
    }
