module Arkham.Event.Cards.TheCircleUndone where

import Arkham.Agenda.AdvancementReason
import Arkham.Asset.Uses qualified as Uses
import Arkham.Criteria qualified as Criteria
import Arkham.Event.Cards.Import
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher qualified as Matcher
import Arkham.Modifier (ModifierType (..))
import Arkham.SlotType

unsolvedCase :: CardDef
unsolvedCase =
  (event "05010" "Unsolved Case" 4 Neutral)
    { cdCardTraits = setFromList [Insight, Mystery]
    , cdCardSubType = Just Weakness
    , cdLevel = Nothing
    , cdCardInHandEffects = True
    }

lodgeDebts :: CardDef
lodgeDebts =
  (event "05012" "Lodge \"Debts\"" 10 Neutral)
    { cdCardTraits = singleton Pact
    , cdCardSubType = Just Weakness
    , cdLevel = Nothing
    , cdCardInHandEffects = True
    }

darkInsight :: CardDef
darkInsight =
  signature "05004"
    $ (event "05014" "Dark Insight" 2 Neutral)
      { cdCardTraits = singleton Insight
      , cdFastWindow =
          Just
            $ OrWindowMatcher
              [ DrawCard
                  #when
                  (affectsOthers $ InvestigatorAt YourLocation)
                  (basic $ NonPeril <> oneOf [IsEncounterCard, WeaknessCard])
                  AnyDeck
              , DrawCard #when You (basic $ oneOf [IsEncounterCard, WeaknessCard]) AnyDeck
              ]
      }

imDoneRunnin :: CardDef
imDoneRunnin =
  signature "05005"
    $ (event "05016" "\"I'm done runnin'!\"" 0 Neutral)
      { cdSkills = [#combat, #agility, #wild]
      , cdCardTraits = singleton Spirit
      , cdFastWindow = Just $ DuringTurn You
      }

mystifyingSong :: CardDef
mystifyingSong =
  signature "05006"
    $ (event "05018" "Mystifying Song" 3 Neutral)
      { cdSkills = [#wild, #wild]
      , cdCardTraits = setFromList [Spell, Song]
      , cdFastWindow = Just $ AgendaWouldAdvance #when DoomThreshold AnyAgenda
      , cdAlternateCardCodes = ["99002"]
      }

interrogate :: CardDef
interrogate =
  (event "05020" "Interrogate" 2 Guardian)
    { cdSkills = [#combat, #intellect]
    , cdCardTraits = setFromList [Tactic, Insight]
    , cdCriteria = Just $ exists $ EnemyWithTrait Humanoid <> EnemyAt YourLocation <> CanParleyEnemy You
    , cdActions = [#parley]
    }

delayTheInevitable :: CardDef
delayTheInevitable =
  (event "05021" "Delay the Inevitable" 2 Guardian)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Insight, Spirit, Tactic]
    , cdFastWindow = Just $ DuringTurn You
    }

connectTheDots :: CardDef
connectTheDots =
  (event "05025" "Connect the Dots" 4 Seeker)
    { cdSkills = [#intellect, #intellect]
    , cdCardTraits = singleton Insight
    , cdFastWindow = Just $ DiscoveringLastClue #after You YourLocation
    , cdCriteria =
        Just
          $ exists
          $ LocationWithLowerPrintedShroudThan YourLocation
          <> LocationWithDiscoverableCluesBy You
    }

moneyTalks :: CardDef
moneyTalks =
  (event "05029" "Money Talks" 0 Rogue)
    { cdCardTraits = setFromList [Favor, Gambit]
    , cdFastWindow = Just $ InitiatedSkillTest #when You AnySkillType AnySkillTestValue #any
    }

denyExistence :: CardDef
denyExistence =
  (event "05032" "Deny Existence" 0 Mystic)
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Spell, Paradox]
    , cdFastWindow =
        Just
          $ OrWindowMatcher
            [ WouldDiscardFromHand #when You source
            , LostResources #when You source
            , LostActions #when You source
            , InvestigatorWouldTakeDamage #when You source AnyDamageType
            , InvestigatorWouldTakeHorror #when You source
            ]
    }
 where
  source = SourceMatchesAny [SourceIsEnemyAttack AnyEnemy, Matcher.EncounterCardSource]

eldritchInspiration :: CardDef
eldritchInspiration =
  (event "05033" "Eldritch Inspiration" 0 Mystic)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = setFromList [Spell, Spirit]
    , cdFastWindow =
        Just
          $ WouldTriggerChaosTokenRevealEffectOnCard
            You
            MysticCard
            [#skull, #cultist, #tablet, #elderthing, #autofail]
    , cdAlternateCardCodes = ["60420"]
    }

actOfDesperation :: CardDef
actOfDesperation =
  (event "05037" "Act of Desperation" 0 Survivor)
    { cdSkills = [#combat, #combat]
    , cdCardTraits = setFromList [Tactic, Gambit]
    , cdActions = [#fight]
    , cdAdditionalCost =
        Just $ DiscardFromCost 1 (FromHandOf You <> FromPlayAreaOf You) (#item <> CardFillsSlot HandSlot)
    }

crackTheCase :: CardDef
crackTheCase =
  (event "05110" "Crack the Case" 0 Seeker)
    { cdSkills = [#intellect]
    , cdCardTraits = singleton Insight
    , cdFastWindow = Just $ DiscoveringLastClue #after You YourLocation
    , cdCriteria =
        Just $ exists $ affectsOthers $ can.gain.resources <> InvestigatorAt YourLocation
    }

intelReport :: CardDef
intelReport =
  (event "05111" "Intel Report" 2 Rogue)
    { cdSkills = [#intellect, #intellect]
    , cdCardTraits = setFromList [Favor, Service]
    , cdCriteria =
        Just
          $ Criteria.AnyCriterion
            [ Criteria.ClueOnLocation <> exists (You <> InvestigatorCanDiscoverCluesAt YourLocation)
            , Criteria.CanAffordCostIncrease 2
                <> exists
                  ( You
                      <> InvestigatorCanDiscoverCluesAt
                        (LocationMatchAny [LocationWithDistanceFrom n YourLocation LocationWithAnyClues | n <- [0 .. 2]])
                  )
            ]
    , cdCardInHandEffects = True
    }

banish1 :: CardDef
banish1 =
  (event "05113" "Banish" 2 Mystic)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = singleton Spell
    , cdActions = [#evade]
    , cdLevel = Just 1
    , cdCriteria = Just $ exists $ NonEliteEnemy <> CanEvadeEnemy ThisCard
    }

wellMaintained1 :: CardDef
wellMaintained1 =
  (event "05152" "Well-Maintained" 0 Guardian)
    { cdSkills = [#agility]
    , cdCardTraits = singleton Upgrade
    , cdLevel = Just 1
    , cdFastWindow = Just $ DuringTurn You
    , cdCriteria =
        Just
          $ exists
          $ AssetControlledBy You
          <> #item
          <> NotAsset (AssetWithAttachedEvent $ EventIs "05152")
    }

swiftReflexes :: CardDef
swiftReflexes =
  (event "05156" "Swift Reflexes" 2 Rogue)
    { cdSkills = [#agility, #agility]
    , cdCardTraits = singleton Gambit
    , cdCriteria = Just $ Criteria.Negate Criteria.DuringAction
    , cdFastWindow = Just $ DuringTurn Anyone
    }

bellyOfTheBeast :: CardDef
bellyOfTheBeast =
  (event "05160" "Belly of the Beast" 1 Survivor)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = setFromList [Gambit, Trick]
    , cdFastWindow =
        Just
          $ SkillTestResult #after You (WhileEvadingAnEnemy AnyEnemy)
          $ SuccessResult
          $ atLeast 2
    , cdCriteria = Just $ exists $ YourLocation <> LocationWithDiscoverableCluesBy You
    }

warningShot :: CardDef
warningShot =
  (event "05229" "Warning Shot" 2 Guardian)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Tactic, Trick]
    , cdAdditionalCost = Just $ UseCost (AssetWithTrait Firearm <> AssetControlledBy You) Uses.Ammo 1
    , cdCriteria = Just $ exists (EnemyAt YourLocation <> EnemyCanEnter ConnectedLocation)
    , cdAttackOfOpportunityModifiers = [DoesNotProvokeAttacksOfOpportunity]
    }

telescopicSight3 :: CardDef
telescopicSight3 =
  (event "05230" "Telescopic Sight" 3 Guardian)
    { cdSkills = [#intellect, #combat, #agility]
    , cdCardTraits = setFromList [Item, Upgrade]
    , cdFastWindow = Just $ DuringTurn You
    , cdCriteria = Just $ exists (AssetControlledBy You <> AssetInTwoHandSlots)
    , cdLevel = Just 3
    }

knowledgeIsPower :: CardDef
knowledgeIsPower =
  (event "05231" "Knowledge is Power" 0 Seeker)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = singleton Insight
    , cdFastWindow = Just $ DuringTurn You
    , cdCriteria =
        Just
          $ Criteria.AnyCriterion
            [ exists (AssetControlledBy You <> oneOf [AssetWithTrait Tome, AssetWithTrait Spell])
            , Criteria.ExtendedCardExists
                $ InHandOf You
                <> basic (oneOf [CardWithTrait Tome, CardWithTrait Spell] <> #asset)
                <> CardWithPerformableAbility
                  (AbilityOneOf [AbilityIsActionAbility, AbilityIsFastAbility])
                  [IgnoreAllCosts]
            ]
    }

decoy :: CardDef
decoy =
  (event "05234" "Decoy" 2 Rogue)
    { cdSkills = [#agility, #agility]
    , cdCardTraits = setFromList [Favor, Service]
    , cdActions = [#evade]
    , cdCriteria =
        Just
          $ Criteria.AnyCriterion
            [ exists $ EnemyAt YourLocation <> CanEvadeEnemy ThisCard
            , Criteria.CanAffordCostIncrease 2
                <> exists
                  ( CanEvadeEnemyWithOverride
                      $ Criteria.CriteriaOverride
                      $ Criteria.EnemyCriteria
                      $ Criteria.EnemyExists
                      $ oneOf [EnemyAt (LocationWithDistanceFrom n YourLocation Anywhere) | n <- [1 .. 2]]
                      <> NonEliteEnemy
                  )
            ]
    , cdOverrideActionPlayableIfCriteriaMet = True
    , cdCardInHandEffects = True
    }

fortuneOrFate2 :: CardDef
fortuneOrFate2 =
  (event "05237" "Fortune or Fate" 2 Survivor)
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Fortune, Blessed]
    , cdLimits = [MaxPerGame 1]
    , cdFastWindow = Just $ PlacedDoomCounter #when (SourceIsCancelable AnySource) ScenarioCardTarget
    , cdLevel = Just 2
    }

ghastlyRevelation :: CardDef
ghastlyRevelation =
  (event "05275" "Ghastly Revelation" 0 Seeker)
    { cdSkills = [#intellect, #intellect]
    , cdCardTraits = singleton Spirit
    , cdAttackOfOpportunityModifiers = [DoesNotProvokeAttacksOfOpportunity]
    }

smallFavor :: CardDef
smallFavor =
  (event "05277" "Small Favor" 2 Rogue)
    { cdSkills = [#combat, #combat]
    , cdCardTraits = setFromList [Favor, Service]
    , cdCriteria =
        Just
          $ Criteria.CanDealDamage
          <> Criteria.AnyCriterion
            [ exists $ EnemyAt YourLocation <> NonEliteEnemy
            , exists (oneOf [EnemyAt (LocationWithDistanceFrom n YourLocation Anywhere) | n <- [1 .. 2]])
                <> Criteria.CanAffordCostIncrease 2
            ]
    , cdCardInHandEffects = True
    }

denyExistence5 :: CardDef
denyExistence5 =
  (event "05280" "Deny Existence" 0 Mystic)
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Spell, Paradox]
    , cdFastWindow =
        Just
          $ OrWindowMatcher
            [ WouldDiscardFromHand #when You source
            , LostResources #when You source
            , LostActions #when You source
            , InvestigatorWouldTakeDamage #when You source AnyDamageType
            , InvestigatorWouldTakeHorror #when You source
            ]
    , cdLevel = Just 5
    }
 where
  source = SourceMatchesAny [SourceIsEnemyAttack AnyEnemy, Matcher.EncounterCardSource]

trialByFire :: CardDef
trialByFire =
  (event "05281" "Trial by Fire" 3 Survivor)
    { cdSkills = [#wild]
    , cdCardTraits = singleton Spirit
    , cdFastWindow = Just $ DuringTurn You
    }

baitAndSwitch3 :: CardDef
baitAndSwitch3 =
  (event "05282" "Bait and Switch" 1 Survivor)
    { cdSkills = [#intellect, #agility, #agility]
    , cdCardTraits = setFromList [Trick]
    , cdActions = [#evade]
    , cdLevel = Just 3
    , cdCriteria =
        Just
          $ exists
          $ oneOf
            [ EnemyAt YourLocation <> CanEvadeEnemy ThisCard
            , CanEvadeEnemyWithOverride
                $ Criteria.CriteriaOverride
                $ Criteria.enemyExists
                $ EnemyAt (ConnectedFrom YourLocation)
                <> NonEliteEnemy
            ]
    , cdOverrideActionPlayableIfCriteriaMet = True
    }

soothingMelody :: CardDef
soothingMelody =
  (event "05314" "Soothing Melody" 0 Guardian)
    { cdSkills = [#willpower, #intellect, #agility]
    , cdCardTraits = singleton Spell
    , cdCriteria =
        Just
          $ Criteria.AnyCriterion
            [ exists (HealableInvestigator ThisCard #damage $ InvestigatorAt YourLocation)
            , exists (HealableInvestigator ThisCard #horror $ InvestigatorAt YourLocation)
            , exists (HealableAsset ThisCard #damage $ AssetAt YourLocation <> AllyAsset)
            , exists (HealableAsset ThisCard #horror $ AssetAt YourLocation <> AllyAsset)
            , Criteria.CanDrawCards
            ]
    , cdKeywords = setFromList [Keyword.Bonded 3 "05313", Keyword.Bonded 3 "54002"]
    , cdLevel = Nothing
    }

iveHadWorse2 :: CardDef
iveHadWorse2 =
  (event "05315" "\"I've had worse…\"" 0 Guardian)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = singleton Spirit
    , cdFastWindow = Just $ DealtDamageOrHorror #when (SourceIsCancelable AnySource) You
    , cdLevel = Just 2
    }

bloodRite :: CardDef
bloodRite =
  (event "05317" "Blood-Rite" 0 Seeker)
    { cdSkills = [#willpower, #intellect, #combat]
    , cdCardTraits = singleton Spell
    , cdLevel = Nothing
    , cdKeywords = setFromList [Keyword.Bonded 3 "05316", Keyword.Bonded 3 "54004"]
    , cdAttackOfOpportunityModifiers = [DoesNotProvokeAttacksOfOpportunity]
    }

glimpseTheUnthinkable5 :: CardDef
glimpseTheUnthinkable5 =
  (event "05318" "Glimpse the Unthinkable" 1 Seeker)
    { cdSkills = [#intellect, #intellect, #intellect]
    , cdCardTraits = singleton Insight
    , cdLevel = Just 5
    , cdCriteria = Just $ Criteria.AnyCriterion [Criteria.CanDrawCards, Criteria.CanManipulateDeck]
    }

youOweMeOne :: CardDef
youOweMeOne =
  (event "05319" "\"You owe me one!\"" 0 Rogue)
    { cdSkills = [#intellect, #combat, #agility]
    , cdCardTraits = setFromList [Favor, Gambit]
    , cdCriteria = Just $ exists (affectsOthers $ NotInvestigator You <> HandWith AnyCards)
    }

lure2 :: CardDef
lure2 =
  (event "05323" "Lure" 1 Survivor)
    { cdSkills = [#agility, #agility]
    , cdCardTraits = singleton Trick
    , cdLevel = Just 2
    }

eucatastrophe3 :: CardDef
eucatastrophe3 =
  (event "05324" "Eucatastrophe" 2 Survivor)
    { cdSkills = [#wild, #wild]
    , cdCardTraits = setFromList [Fortune, Blessed]
    , cdFastWindow = Just $ RevealChaosToken #when You WouldReduceYourSkillValueToZero
    , cdAlternateCardCodes = ["01692"]
    , cdLevel = Just 3
    }
