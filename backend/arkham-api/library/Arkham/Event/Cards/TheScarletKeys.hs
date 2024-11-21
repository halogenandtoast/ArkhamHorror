module Arkham.Event.Cards.TheScarletKeys where

import Arkham.Asset.Uses qualified as Uses
import Arkham.Criteria qualified as Criteria
import Arkham.Customization
import Arkham.Event.Cards.Import
import Arkham.Keyword qualified as Keyword
import Arkham.Modifier (ModifierType (..))

wordOfWoe :: CardDef
wordOfWoe =
  signature "09011"
    $ (event "09012" "Word of Woe" 2 Neutral)
      { cdCardTraits = setFromList [Pact]
      , cdFastWindow = Just $ DuringTurn You
      , cdCriteria =
          Just
            $ exists
              (AssetControlledBy You <> AssetWithPerformableAbility AbilityIsActionAbility [IgnoreAllCosts])
      }

wordOfWeal :: CardDef
wordOfWeal =
  signature "09011"
    $ (event "09013" "Word of Weal" 0 Neutral)
      { cdCardTraits = setFromList [Pact]
      , cdFastWindow = Just $ InitiatedSkillTest #when You #any #any $ SkillTestOnAsset AssetWithAnyDoom
      }

customModifications :: CardDef
customModifications =
  (event "09023" "Custom Modifications" 3 Guardian)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Upgrade, Supply]
    , cdCriteria =
        Just $ exists $ AssetControlledBy You <> #firearm <> not_ (AssetWithAttachedEvent $ EventIs "09023")
    , cdCardInHandEffects = True
    , cdKeywords = setFromList [Keyword.Customizable]
    , cdCustomizations =
        mapFromList
          [ (NotchedSight, 1)
          , (ExtendedStock, 2)
          , (Counterbalance, 2)
          , (LeatherGrip, 3)
          , (ExtendedMagazine, 3)
          , (QuicksilverBullets, 4)
          ]
    }

bolas :: CardDef
bolas =
  (event "09025" "Bolas" 1 Guardian)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Tactic]
    , cdActions = [#evade]
    }

breachTheDoor :: CardDef
breachTheDoor =
  (event "09026" "Breach the Door" 2 Guardian)
    { cdSkills = [#intellect, #combat]
    , cdCardTraits = setFromList [Insight, Tactic, Police]
    }

grievousWound :: CardDef
grievousWound =
  (event "09027" "Grievous Wound" 1 Guardian)
    { cdSkills = [#combat, #combat]
    , cdCardTraits = setFromList [Tactic]
    , cdFastWindow =
        Just $ EnemyAttackedSuccessfully #after You (SourceIsAsset $ AssetWithTrait Melee) NonEliteEnemy
    }

motivationalSpeech :: CardDef
motivationalSpeech =
  (event "09028" "Motivational Speech" 0 Guardian)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = setFromList [Spirit]
    , cdActions = [#parley]
    }

oneInTheChamber :: CardDef
oneInTheChamber =
  (event "09029" "One in the Chamber" 1 Guardian)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Fortune, Tactic]
    , cdFastWindow =
        Just
          $ AttackOrEffectSpentLastUse
            #after
            AnySource
            (AssetTargetMatches $ AssetWithTrait Firearm <> AssetControlledBy You)
            Uses.Ammo
    }

preparedForTheWorst2 :: CardDef
preparedForTheWorst2 =
  (event "09036" "Prepared for the Worst" 0 Guardian)
    { cdSkills = [#intellect, #combat, #agility]
    , cdCardTraits = singleton Tactic
    , cdCriteria = Just $ exists $ affectsOthers $ can.search.deck <> InvestigatorAt YourLocation
    , cdLevel = Just 2
    }

everVigilant4 :: CardDef
everVigilant4 =
  (event "09039" "Ever Vigilant" 0 Guardian)
    { cdSkills = [#willpower, #intellect, #intellect]
    , cdCardTraits = singleton Tactic
    , cdLevel = Just 4
    , cdCriteria =
        Just
          $ Criteria.PlayableCardExistsWithCostReduction (Reduce 1)
          $ #asset
          <> InHandOf (affectsOthers $ InvestigatorAt YourLocation)
    }

theRavenQuill :: CardDef
theRavenQuill =
  (event "09042" "The Raven Quill" 3 Seeker)
    { cdSkills = [#intellect, #intellect]
    , cdCardTraits = setFromList [Item, Relic, Upgrade]
    , cdUnique = True
    , cdKeywords = setFromList [Keyword.Customizable]
    , cdCriteria =
        Just
          $ oneOf
            [ Criteria.ChosenCustomizationCardIsInPlay
            , Criteria.HasCustomization SupernaturalRecord
                <> oneOf
                  [ can.search.deck You
                  , exists
                      $ PlayableCard (UnpaidCost NoAction)
                      $ oneOf [InHandOf You, InDiscardOf You]
                      <> ChosenViaCustomization IsThisCard
                  ]
            ]
    , cdCustomizations =
        mapFromList
          [ (ChoicePlaceholder, 0)
          , (LivingQuill, 1)
          , (SpectralBinding, 1)
          , (MysticVane, 2)
          , (EndlessInkwell, 2)
          , (EnergySap, 2)
          , (InterwovenInk, 3)
          , (SupernaturalRecord, 4)
          ]
    }

bizarreDiagnosis :: CardDef
bizarreDiagnosis =
  (event "09046" "Bizarre Diagnosis" 0 Seeker)
    { cdSkills = [#intellect, #intellect]
    , cdCardTraits = setFromList [Insight, Science]
    , cdCriteria =
        Just
          $ exists (You <> oneOf [InvestigatorWithAnyClues <> InvestigatorAt Anywhere, canParallelRexClues])
    }

captivatingDiscovery :: CardDef
captivatingDiscovery =
  (event "09047" "Captivating Discovery" 1 Seeker)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = setFromList [Insight]
    , cdCriteria = Just $ can.search.deck You
    }

mapTheArea :: CardDef
mapTheArea =
  (event "09048" "Map the Area" 1 Seeker)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = setFromList [Insight, Tactic]
    , cdActions = [#investigate]
    }

existentialRiddle1 :: CardDef
existentialRiddle1 =
  (event "09052" "Existential Riddle" 1 Seeker)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = setFromList [Insight, Paradox]
    , cdActions = [#parley]
    , cdCriteria = Just $ exists (EnemyAt YourLocation <> NonEliteEnemy <> CanParleyEnemy You)
    , cdLevel = Just 1
    }

guidance1 :: CardDef
guidance1 =
  (event "09053" "Guidance" 0 Seeker)
    { cdSkills = [#willpower, #wild]
    , cdCardTraits = setFromList [Insight]
    , cdCriteria = Just $ exists (affectsOthers $ InvestigatorAt YourLocation <> NotYou <> YetToTakeTurn)
    , cdAttackOfOpportunityModifiers = [DoesNotProvokeAttacksOfOpportunity]
    , cdLevel = Just 1
    }

friendsInLowPlaces :: CardDef
friendsInLowPlaces =
  (event "09060" "Friends in Low Places" 0 Rogue)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = setFromList [Favor]
    , cdCardInHandEffects = True
    , cdCriteria = Just $ can.manipulate.deck You
    , cdKeywords = setFromList [Keyword.Customizable]
    , cdCustomizations =
        mapFromList
          [ (ChoicePlaceholder, 0)
          , (Helpful, 1)
          , (Versatile, 2)
          , (Bolstering, 2)
          , (Clever, 2)
          , (Prompt, 2)
          , (Experienced, 3)
          , (Swift, 3)
          ]
    }

honedInstinct :: CardDef
honedInstinct =
  (event "09061" "Honed Instinct" 1 Rogue)
    { cdSkills = [#agility]
    , cdCardTraits = setFromList [Gambit]
    , cdCardInHandEffects = True
    , cdKeywords = setFromList [Keyword.Customizable]
    , cdLimits = [MaxPerRound 1]
    , cdFastWindow =
        Just
          $ oneOf
            [ AgendaAdvances #after AnyAgenda
            , ActAdvances #after AnyAct
            , SkillTestResult #after You AnySkillTest $ SuccessResult (atLeast 3)
            , WindowWhen (Criteria.HasCustomization ReflexResponse) $ InvestigatorTakeDamage #after You AnySource
            , WindowWhen (Criteria.HasCustomization ReflexResponse) $ InvestigatorTakeHorror #after You AnySource
            , WindowWhen (Criteria.HasCustomization SituationalAwareness) $ LocationEntersPlay #after Anywhere
            , WindowWhen (Criteria.HasCustomization SituationalAwareness) $ RevealLocation #after Anyone Anywhere
            , WindowWhen (Criteria.HasCustomization KillerInstinct) $ EnemyEngaged #after You AnyEnemy
            , WindowWhen (Criteria.HasCustomization GutReaction) $ EntersThreatArea #after You #treachery
            , WindowWhen (Criteria.HasCustomization MuscleMemory) $ PlayCard #after You #asset
            ]
    , cdCustomizations =
        mapFromList
          [ (ReflexResponse, 1)
          , (SituationalAwareness, 1)
          , (KillerInstinct, 1)
          , (GutReaction, 1)
          , (MuscleMemory, 1)
          , (SharpenedTalent, 2)
          , (ImpulseControl, 3)
          , (ForceOfHabit, 5)
          ]
    }

hiddenPocket :: CardDef
hiddenPocket =
  (event "09065" "Hidden Pocket" 1 Rogue)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = setFromList [Upgrade, Illicit]
    , cdFastWindow = Just $ DuringTurn You
    , cdCriteria = Just $ exists $ AssetControlledBy You <> mapOneOf AssetWithTrait [Clothing, Armor]
    }

hitAndRun :: CardDef
hitAndRun =
  (event "09066" "Sleight of Hand" 1 Rogue)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Tactic, Trick]
    , cdFastWindow = Just $ DuringTurn You
    , cdCriteria = Just $ Criteria.PlayableCardExists PaidCost $ InHandOf You <> #ally <> #asset
    }

illTakeThat :: CardDef
illTakeThat =
  (event "09067" "\"I'll take that!\"" 0 Rogue)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = setFromList [Trick, Upgrade, Illicit]
    , cdFastWindow =
        Just
          $ oneOf
            [ SkillTestResult #when You (WhileInvestigating Anywhere) #success
            , SkillTestResult #when You (WhileEvadingAnEnemy $ EnemyWithTrait Humanoid) #success
            ]
    , cdCriteria =
        Just
          $ Criteria.PlayableCardExistsWithCostReduction ReduceBySuccessAmount
          $ InHandOf You
          <> #item
          <> #asset
    }

kickingTheHornetsNest :: CardDef
kickingTheHornetsNest =
  (event "09068" "Kicking the Hornets Nest" 0 Rogue)
    { cdSkills = [#intellect, #combat]
    , cdCardTraits = setFromList [Gambit, Tactic]
    , cdCriteria = can.target.encounterDeck
    }

quickGetaway :: CardDef
quickGetaway =
  (event "09069" "Quick Getaway" 2 Rogue)
    { cdSkills = [#agility, #agility]
    , cdCardTraits = setFromList [Trick]
    , cdActions = [#evade]
    , cdFastWindow =
        Just $ EnemyAttacks #when You AnyEnemyAttack (EnemyWithEvade <> EnemyIsEngagedWith You)
    }

breakingAndEntering2 :: CardDef
breakingAndEntering2 =
  (event "09074" "Breaking and Entering" 2 Rogue)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = setFromList [Trick]
    , cdActions = [#investigate]
    , cdAttackOfOpportunityModifiers = [DoesNotProvokeAttacksOfOpportunity]
    , cdLevel = Just 2
    }

cleanSneak4 :: CardDef
cleanSneak4 =
  (event "09078" "Clean Sneak" 0 Rogue)
    { cdSkills = [#agility, #agility]
    , cdCardTraits = setFromList [Gambit, Trick]
    , cdFastWindow = Just $ DuringTurn You
    , cdCriteria =
        Just
          $ exists (EnemyAt YourLocation <> not_ IsSwarm <> ExhaustedEnemy)
          <> oneOf
            [ youExist (oneOf [can.gain.resources, can.draw.cards])
            , exists
                (EnemyAt YourLocation <> not_ IsSwarm <> ExhaustedEnemy <> EnemyCanBeDamagedBySource ThisCard)
            , exists (LocationWithDiscoverableCluesBy You)
            ]
    , cdLevel = Just 4
    }

powerWord :: CardDef
powerWord =
  (event "09081" "Power Word" 3 Mystic)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Spell]
    , cdFastWindow = Just $ DuringTurn You
    , cdCriteria = Just $ exists $ EnemyAt YourLocation <> NonEliteEnemy
    , cdKeywords = setFromList [Keyword.Customizable]
    , cdCustomizations =
        mapFromList
          [ (Betray, 1)
          , (Mercy, 1)
          , (Confess, 1)
          , (Distract, 1)
          , (GreaterControl, 2)
          , (Bonded, 3)
          , (Tonguetwister, 3)
          , (ThriceSpoken, 3)
          ]
    , cdTags = ["parley"]
    }

eldritchInitiation :: CardDef
eldritchInitiation =
  (event "09086" "Eldritch Initiation" 1 Mystic)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = setFromList [Ritual]
    }

explosiveWard :: CardDef
explosiveWard =
  (event "09087" "Explosive Ward" 0 Mystic)
    { cdSkills = [#combat, #combat]
    , cdCardTraits = setFromList [Spell]
    , cdBeforeEffect = True
    , cdCost = Just $ MaxDynamicCost $ EmptySlotsCalculation You #arcane
    , cdAttackOfOpportunityModifiers = [DoesNotProvokeAttacksOfOpportunity]
    , cdCriteria = Just $ exists $ EnemyIsEngagedWith You <> NonEliteEnemy
    }

stringOfCurses :: CardDef
stringOfCurses =
  (event "09088" "String of Curses" 1 Mystic)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Spell]
    , cdActions = [#parley]
    , cdCriteria =
        Just
          $ exists
          $ NonEliteEnemy
          <> EnemyAt YourLocation
          <> oneOf [EnemyCanBeEvadedBy ThisCard, EnemyWithAnyDoom <> EnemyCanBeDefeatedBy ThisCard]
    }

moonlightRitual2 :: CardDef
moonlightRitual2 =
  (event "09093" "Moonlight Ritual" 0 Mystic)
    { cdSkills = [#intellect, #agility, #wild]
    , cdCardTraits = setFromList [Spell, Insight]
    , cdFastWindow = Just $ DuringTurn You
    , cdCriteria =
        Just (exists $ TargetWithDoom <> TargetAtLocation YourLocation <> not_ (TargetWithTrait Elite))
    , cdLevel = Just 2
    }

uncageTheSoul3 :: CardDef
uncageTheSoul3 =
  (event "09095" "Uncage the Soul" 0 Mystic)
    { cdSkills = [#willpower, #wild]
    , cdCardTraits = singleton Spirit
    , cdCriteria =
        Just
          $ Criteria.PlayableCardExistsWithCostReduction (Reduce 3)
          $ oneOf
            [ InHandOf You <> basic (oneOf [#spell, #ritual])
            , InDiscardOf You <> basic (oneOf [#spell, #ritual])
            , CardIsAsset $ AssetControlledBy You <> oneOf [#spell, #ritual]
            ]
    }

makeshiftTrap :: CardDef
makeshiftTrap =
  (event "09100" "Makeshift Trap" 1 Survivor)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = setFromList [Improvised, Trap]
    , cdUses = Uses.Uses Uses.Time (Fixed 2)
    , cdCardInHandEffects = True
    , cdKeywords = setFromList [Keyword.Customizable]
    , cdCustomizations =
        mapFromList
          [ (ImprovedTimer, 1)
          , (Tripwire, 1)
          , (Simple, 2)
          , (Poisonous, 2)
          , (RemoteConfiguration, 2)
          , (Net, 3)
          , (ExplosiveDevice, 4)
          ]
    }

endOfTheRoad :: CardDef
endOfTheRoad =
  (event "09104" "End of the Road" 0 Survivor)
    { cdCardTraits = setFromList [Insight, Spirit]
    , cdFastWindow = Just $ DuringTurn You
    , cdCriteria = Just $ exists FinalAgenda
    }

exploitWeakness :: CardDef
exploitWeakness =
  (event "09105" "Exploit Weakness" 2 Survivor)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Tactic, Trick]
    , cdFastWindow = Just $ WouldRevealChaosToken #when You
    , cdCriteria =
        Just
          $ Criteria.DuringSkillTest
          $ oneOf [WhileAttackingAnEnemy AnyEnemy, WhileEvadingAnEnemy AnyEnemy]
          <> SkillTestWithDifficulty (static 0)
    }

makingPreparations :: CardDef
makingPreparations =
  (event "09106" "Making Preparations" 0 Survivor)
    { cdCardTraits = setFromList [Dilemma, Tactic]
    , cdRevelation = IsRevelation
    , cdCost = Nothing
    , cdLimits = [MaxPerTraitPerRound Dilemma 2]
    , cdCriteria = Just Criteria.Never
    }

predatorOrPrey :: CardDef
predatorOrPrey =
  (event "09107" "Predator or Prey" 0 Survivor)
    { cdCardTraits = setFromList [Dilemma, Tactic]
    , cdRevelation = IsRevelation
    , cdCost = Nothing
    , cdLimits = [MaxPerTraitPerRound Dilemma 2]
    , cdCriteria = Just Criteria.Never
    }

shedALight :: CardDef
shedALight =
  (event "09108" "Shed a Light" 2 Survivor)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = setFromList [Insight, Trick]
    , cdFastWindow = Just $ WouldRevealChaosToken #when You
    , cdCriteria =
        Just
          $ Criteria.DuringSkillTest
          $ WhileInvestigating Anywhere
          <> SkillTestWithDifficulty (static 0)
    }

atACrossroads1 :: CardDef
atACrossroads1 =
  (event "09109" "At a Crossroads" 0 Survivor)
    { cdCardTraits = setFromList [Dilemma, Insight]
    , cdRevelation = IsRevelation
    , cdCost = Nothing
    , cdLimits = [MaxPerTraitPerRound Dilemma 2]
    , cdCriteria = Just Criteria.Never
    , cdLevel = Just 1
    }

lifeline1 :: CardDef
lifeline1 =
  (event "09110" "Lifeline" 1 Survivor)
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Fortune]
    , cdLimits = [MaxPerTurn 1]
    , cdFastWindow =
        Just $ TurnWouldEnd #when (affectsOthers $ InvestigatorWithAnyFailedSkillTestsThisTurn)
    , cdLevel = Just 1
    }

natureOfTheBeast1 :: CardDef
natureOfTheBeast1 =
  (event "09111" "Nature of the Beast" 0 Survivor)
    { cdCardTraits = setFromList [Dilemma, Insight]
    , cdRevelation = IsRevelation
    , cdCost = Nothing
    , cdLimits = [MaxPerTraitPerRound Dilemma 2]
    , cdCriteria = Just Criteria.Never
    , cdLevel = Just 1
    }

heedTheDream2 :: CardDef
heedTheDream2 =
  (event "09115" "Heed the Dream" 0 Survivor)
    { cdCardTraits = setFromList [Augury, Dilemma]
    , cdRevelation = IsRevelation
    , cdCost = Nothing
    , cdLimits = [MaxPerTraitPerRound Dilemma 2]
    , cdCriteria = Just Criteria.Never
    , cdLevel = Just 1
    }

salvage2 :: CardDef
salvage2 =
  (event "09116" "Salvage" 0 Survivor)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Insight]
    , cdCriteria =
        Just
          $ exists
          $ InDiscardOf You
          <> basic #item
          <> oneOf [basic CardWithNonZeroCost, PlayableCard (UnpaidCost NoAction) #any]
    , cdLevel = Just 2
    }

fickleFortune3 :: CardDef
fickleFortune3 =
  (event "09118" "Fickle Fortune" 0 Survivor)
    { cdCardTraits = setFromList [Dilemma, Fortune]
    , cdRevelation = IsRevelation
    , cdCost = Nothing
    , cdLimits = [MaxPerTraitPerRound Dilemma 2]
    , cdCriteria = Just Criteria.Never
    , cdLevel = Just 1
    }

refine :: CardDef
refine =
  (event "09121" "Refine" 3 Neutral)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = setFromList [Supply, Double]
    , cdCriteria = Just $ exists $ OwnedBy You <> basic CardWithAvailableCustomization
    , cdAdditionalCost = Just (ActionCost 1)
    }

quantumParadox :: CardDef
quantumParadox =
  (event "09125" "Quantum Paradox" 0 Neutral)
    { cdCardTraits = setFromList [Paradox]
    , cdCardSubType = Just BasicWeakness
    , cdLevel = Nothing
    , cdCardInHandEffects = True
    , cdAdditionalCost = Just $ HandDiscardCost 4 #any
    , cdDeckRestrictions = [OnlyClass Seeker]
    }

payYourDue :: CardDef
payYourDue =
  (event "09126" "Pay Your Due" 10 Neutral)
    { cdCardTraits = setFromList [Paradox]
    , cdCardSubType = Just BasicWeakness
    , cdLevel = Nothing
    , cdCardInHandEffects = True
    , cdAdditionalCost = Just $ AdditionalActionsCostThatReducesResourceCostBy 5 mempty
    , cdDeckRestrictions = [OnlyClass Seeker]
    }

underprepared :: CardDef
underprepared =
  (event "09128" "Underprepared" 1 Neutral)
    { cdCardTraits = setFromList [Blunder]
    , cdCardSubType = Just BasicWeakness
    , cdLevel = Nothing
    , cdCardInHandEffects = True
    , cdDeckRestrictions = [OnlyClass Survivor]
    , cdCriteria =
        Just $ youExist (HandWith (LengthIs $ atMost 1) <> InvestigatorWithResources (atMost 1))
    }
