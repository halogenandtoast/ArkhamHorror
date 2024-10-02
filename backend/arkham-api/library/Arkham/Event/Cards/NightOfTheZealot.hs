module Arkham.Event.Cards.NightOfTheZealot where

import Arkham.Criteria qualified as Criteria
import Arkham.Event.Cards.Import

onTheLam :: CardDef
onTheLam =
  signature "01003"
    $ (event "01010" "On the Lam" 1 Neutral)
      { cdCardTraits = setFromList [Tactic]
      , cdSkills = [#intellect, #agility, #wild, #wild]
      , cdFastWindow = Just (TurnBegins #after You)
      , cdAlternateCardCodes = ["01510"]
      }

darkMemory :: CardDef
darkMemory =
  (event "01013" "Dark Memory" 2 Neutral)
    { cdCardTraits = setFromList [Spell]
    , cdCardSubType = Just Weakness
    , cdLevel = Nothing
    , cdCardInHandEffects = True
    , cdAlternateCardCodes = ["01513"]
    }

evidence :: CardDef
evidence =
  (event "01022" "Evidence!" 1 Guardian)
    { cdSkills = [#intellect, #intellect]
    , cdCardTraits = setFromList [Insight]
    , cdFastWindow = Just $ EnemyDefeated #after You ByAny AnyEnemy
    , cdCriteria = Just canDiscoverCluesAtYourLocation
    , cdAlternateCardCodes = ["01522"]
    }

dodge :: CardDef
dodge =
  (event "01023" "Dodge" 1 Guardian)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = setFromList [Tactic]
    , cdFastWindow =
        Just
          $ EnemyAttacks
            #when
            (affectsOthers $ InvestigatorAt YourLocation)
            (CancelableEnemyAttack AnyEnemyAttack)
            AnyEnemy
    , cdAlternateCardCodes = ["01523", "60113"]
    }

dynamiteBlast :: CardDef
dynamiteBlast =
  (event "01024" "Dynamite Blast" 5 Guardian)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Tactic]
    , cdAlternateCardCodes = ["01524"]
    , cdCriteria = Just Criteria.CanDealDamage
    }

extraAmmunition1 :: CardDef
extraAmmunition1 =
  (event "01026" "Extra Ammunition" 2 Guardian)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Supply]
    , cdLevel = Just 1
    , cdCriteria =
        Just
          $ exists
          $ AssetControlledBy (affectsOthers $ InvestigatorAt YourLocation)
          <> AssetWithTrait Firearm
    , cdAlternateCardCodes = ["01526"]
    }

mindOverMatter :: CardDef
mindOverMatter =
  (event "01036" "Mind over Matter" 1 Seeker)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Insight]
    , cdFastWindow = Just $ DuringTurn You
    , cdAlternateCardCodes = ["01536"]
    }

workingAHunch :: CardDef
workingAHunch =
  (event "01037" "Working a Hunch" 2 Seeker)
    { cdSkills = [#intellect, #intellect]
    , cdCardTraits = setFromList [Insight]
    , cdFastWindow = Just $ DuringTurn You
    , cdCriteria = Just canDiscoverCluesAtYourLocation
    , cdAlternateCardCodes = ["01537"]
    }

barricade :: CardDef
barricade =
  (event "01038" "Barricade" 0 Seeker)
    { cdSkills = [#willpower, #intellect, #agility]
    , cdCardTraits = setFromList [Insight, Tactic]
    , cdAlternateCardCodes = ["01538"]
    }

crypticResearch4 :: CardDef
crypticResearch4 =
  (event "01043" "Cryptic Research" 0 Seeker)
    { cdCardTraits = setFromList [Insight]
    , cdLevel = Just 4
    , cdFastWindow = Just $ DuringTurn You
    , cdAlternateCardCodes = ["01543"]
    , cdCriteria =
        Just $ exists $ affectsOthers $ InvestigatorAt YourLocation <> can.draw.cards FromPlayerCardEffect
    }

elusive :: CardDef
elusive =
  (event "01050" "Elusive" 2 Rogue)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = singleton Tactic
    , cdFastWindow = Just $ DuringTurn You
    , cdCriteria =
        Just
          $ Criteria.AnyCriterion
            [ exists EnemyEngagedWithYou
            , Criteria.TabooCriteria
                TabooList19
                ( Criteria.CanMoveTo
                    $ AccessibleFrom YourLocation
                    <> LocationWithoutEnemies
                )
                ( Criteria.CanMoveTo
                    $ RevealedLocation
                    <> LocationWithoutEnemies
                )
            ]
    , cdAlternateCardCodes = ["01550"]
    }

backstab :: CardDef
backstab =
  (event "01051" "Backstab" 3 Rogue)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Tactic]
    , cdActions = [#fight]
    , cdAlternateCardCodes = ["01551"]
    }

sneakAttack :: CardDef
sneakAttack =
  (event "01052" "Sneak Attack" 2 Rogue)
    { cdSkills = [#intellect, #combat]
    , cdCardTraits = setFromList [Tactic]
    , cdCriteria =
        Just
          $ exists (EnemyAt YourLocation <> ExhaustedEnemy <> EnemyCanBeDamagedBySource ThisCard)
          <> Criteria.CanDealDamage
    , cdAlternateCardCodes = ["01552"]
    }

sureGamble3 :: CardDef
sureGamble3 =
  (event "01056" "Sure Gamble" 2 Rogue)
    { cdCardTraits = setFromList [Fortune, Insight]
    , cdFastWindow = Just $ RevealChaosToken #when You WithNegativeModifier
    , cdLevel = Just 3
    , cdAlternateCardCodes = ["01556"]
    }

hotStreak4 :: CardDef
hotStreak4 =
  (event "01057" "Hot Streak" 3 Rogue)
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Fortune]
    , cdLevel = Just 4
    , cdAlternateCardCodes = ["01557"]
    }

drawnToTheFlame :: CardDef
drawnToTheFlame =
  (event "01064" "Drawn to the Flame" 0 Mystic)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = setFromList [Insight]
    , cdAlternateCardCodes = ["01564"]
    , cdCriteria = Just $ exists $ You <> can.target.encounterDeck
    }

wardOfProtection :: CardDef
wardOfProtection =
  (event "01065" "Ward of Protection" 1 Mystic)
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Spell, Spirit]
    , cdFastWindow =
        Just $ DrawCard #when You (CanCancelRevelationEffect $ basic NonWeaknessTreachery) EncounterDeck
    , cdAlternateCardCodes = ["01565"]
    }

blindingLight :: CardDef
blindingLight =
  (event "01066" "Blinding Light" 2 Mystic)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = setFromList [Spell]
    , cdActions = [#evade]
    , cdAlternateCardCodes = ["01566"]
    }

mindWipe1 :: CardDef
mindWipe1 =
  (event "01068" "Mind Wipe" 1 Mystic)
    { cdSkills = [#willpower, #combat]
    , cdCardTraits = setFromList [Spell]
    , cdLevel = Just 1
    , cdFastWindow = Just $ PhaseBegins #after AnyPhase
    , cdCriteria = Just $ exists $ EnemyAt YourLocation <> NonEliteEnemy
    , cdAlternateCardCodes = ["01568"]
    }

blindingLight2 :: CardDef
blindingLight2 =
  (event "01069" "Blinding Light" 1 Mystic)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = setFromList [Spell]
    , cdActions = [#evade]
    , cdLevel = Just 2
    , cdAlternateCardCodes = ["01569"]
    }

cunningDistraction :: CardDef
cunningDistraction =
  (event "01078" "Cunning Distraction" 5 Survivor)
    { cdSkills = [#willpower, #wild]
    , cdCardTraits = setFromList [Tactic]
    , cdActions = [#evade]
    , cdAlternateCardCodes = ["01578"]
    }

lookWhatIFound :: CardDef
lookWhatIFound =
  (event "01079" "\"Look what I found!\"" 2 Survivor)
    { cdSkills = [#intellect, #intellect]
    , cdCardTraits = singleton Fortune
    , cdFastWindow =
        Just $ SkillTestResult #after You (WhileInvestigating Anywhere) $ FailureResult $ lessThan 3
    , cdCriteria = Just canDiscoverCluesAtYourLocation
    , cdAlternateCardCodes = ["01579", "60517"]
    }

lucky :: CardDef
lucky =
  (event "01080" "Lucky!" 1 Survivor)
    { cdCardTraits = setFromList [Fortune]
    , cdFastWindow = Just $ WouldHaveSkillTestResult #when You AnySkillTest $ FailureResult AnyValue
    , cdAlternateCardCodes = ["01580"]
    }

closeCall2 :: CardDef
closeCall2 =
  (event "01083" "Close Call" 2 Survivor)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Fortune]
    , cdFastWindow =
        Just $ EnemyEvaded #after Anyone (EnemyAt YourLocation <> NonWeaknessEnemy <> NonEliteEnemy)
    , cdCriteria = Just $ exists $ You <> can.target.encounterDeck
    , cdLevel = Just 2
    , cdAlternateCardCodes = ["01583"]
    }

lucky2 :: CardDef
lucky2 =
  (event "01084" "Lucky!" 1 Survivor)
    { cdCardTraits = setFromList [Fortune]
    , cdFastWindow = Just $ WouldHaveSkillTestResult #when You AnySkillTest $ FailureResult AnyValue
    , cdLevel = Just 2
    , cdAlternateCardCodes = ["01584"]
    }

willToSurvive3 :: CardDef
willToSurvive3 =
  (event "01085" "Will to Survive" 4 Survivor)
    { cdSkills = [#combat, #wild]
    , cdCardTraits = setFromList [Spirit]
    , cdFastWindow = Just $ DuringTurn You
    , cdLevel = Just 3
    , cdAlternateCardCodes = ["01585"]
    }

emergencyCache :: CardDef
emergencyCache =
  (event "01088" "Emergency Cache" 0 Neutral)
    { cdCardTraits = setFromList [Supply]
    , cdAlternateCardCodes = ["01588"]
    , cdCriteria = Just $ exists $ You <> can.gain.resources
    }
