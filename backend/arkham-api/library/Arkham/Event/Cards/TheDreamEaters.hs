module Arkham.Event.Cards.TheDreamEaters where

import Arkham.Criteria qualified as Criteria
import Arkham.Event.Cards.Import
import Arkham.History.Types
import Arkham.Keyword qualified as Keyword

occultEvidence :: CardDef
occultEvidence =
  signature "06002"
    $ (event "06008" "Occult Evidence" 0 Neutral)
      { cdSkills = [#wild]
      , cdCardTraits = setFromList [Insight, Research]
      , cdCardInSearchEffects = True
      , cdCardInHandEffects = True
      , cdCriteria = Just $ Criteria.CanManipulateDeck
      }

astoundingRevelation :: CardDef
astoundingRevelation =
  (event "06023" "Astounding Revelation" 0 Seeker)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Research]
    , cdCost = Nothing
    , cdCardInSearchEffects = True
    , cdKeywords = singleton Keyword.Myriad
    , cdCriteria = Just Criteria.Never
    }

easyMark1 :: CardDef
easyMark1 =
  (event "06026" "Easy Mark" 0 Rogue)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = singleton Trick
    , cdKeywords = singleton Keyword.Myriad
    , cdCriteria = Just $ Criteria.AnyCriterion [Criteria.CanGainResources, Criteria.CanDrawCards]
    , cdLevel = Just 1
    }

stargazing1 :: CardDef
stargazing1 =
  (event "06027" "Stargazing" 0 Mystic)
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Insight, Augury]
    , cdLimits = [MaxPerGame 2]
    , cdCriteria = Just $ Criteria.EncounterDeckWith $ LengthIs (atLeast 10)
    , cdBondedWith = [(1, "06028")]
    , cdLevel = Just 1
    }

theStarsAreRight :: CardDef
theStarsAreRight =
  (event "06028" "The Stars Are Right" 0 Mystic)
    { cdCardTraits = singleton Augury
    , cdKeywords = singleton (Keyword.Bonded 1 "06027")
    , cdCost = Nothing
    , cdRevelation = IsRevelation
    , cdCardType = EncounterEventType
    , cdLevel = Nothing
    }

openGate :: CardDef
openGate =
  (event "06029" "Open Gate" 1 Mystic)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = singleton Spell
    , cdCriteria =
        Just
          $ exists (You <> InvestigatorAt Anywhere)
          <> Criteria.EventCount (lessThan 3) (eventIs openGate)
    , cdFastWindow = Just $ DuringTurn You
    , cdKeywords = singleton Keyword.Myriad
    }

fortuitousDiscovery :: CardDef
fortuitousDiscovery =
  (event "06034" "Fortuitous Discovery" 0 Survivor)
    { cdCardTraits = setFromList [Fortune, Insight]
    , cdActions = [#investigate]
    , cdKeywords = singleton Keyword.Myriad
    , cdCost = Just DiscardAmountCost
    }

firstWatch :: CardDef
firstWatch =
  (event "06110" "First Watch" 1 Guardian)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = setFromList [Tactic]
    , cdFastWindow = Just $ MythosStep WhenAllDrawEncounterCard
    , cdCriteria = Just $ exists $ You <> can.target.encounterDeck
    }

followed :: CardDef
followed =
  (event "06114" "Followed" 2 Rogue)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = singleton Tactic
    , cdActions = [#investigate]
    , cdCriteria = Just $ exists $ EnemyAt YourLocation
    , cdBeforeEffect = True
    }

readTheSigns :: CardDef
readTheSigns =
  (event "06117" "Read the Signs" 2 Mystic)
    { cdSkills = [#willpower, #intellect]
    , cdActions = [#investigate]
    , cdCardTraits = setFromList [Spell]
    }

foolMeOnce1 :: CardDef
foolMeOnce1 =
  (event "06156" "\"Fool me once...\"" 1 Guardian)
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Insight, Tactic]
    , cdFastWindow =
        Just
          $ TreacheryWouldBeDiscarded #when
          $ TreacheryWithResolvedEffectsBy You
          <> TreacheryDiscardedBy You
    , cdLevel = Just 1
    }

letGodSortThemOut :: CardDef
letGodSortThemOut =
  (event "06160" "\"Let God sort them out...\"" 0 Rogue)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Tactic, Fated]
    , cdCriteria = Just $ Criteria.HasHistory TurnHistory You $ DefeatedEnemiesWithTotalHealth (atLeast 6)
    }

swiftReload2 :: CardDef
swiftReload2 =
  (event "06161" "Swift Reload" 3 Rogue)
    { cdSkills = [#agility]
    , cdCardTraits = setFromList [Tactic, Trick]
    , cdFastWindow = Just $ DuringTurn You
    , cdCriteria =
        Just $ exists $ AssetControlledBy You <> AssetWithTrait Firearm <> AssetNotAtUsesX
    , cdLevel = Just 2
    }

etherealForm :: CardDef
etherealForm =
  (event "06164" "Ethereal Form" 2 Mystic)
    { cdSkills = [#willpower, #agility]
    , cdActions = [#evade]
    , cdCardTraits = setFromList [Spell]
    }

scroungeForSupplies :: CardDef
scroungeForSupplies =
  (event "06165" "Scrounge for Supplies" 0 Survivor)
    { cdCardTraits = singleton Fortune
    , cdCriteria = Just $ Criteria.CardInDiscard (Criteria.DiscardOf You) (CardWithLevel 0)
    }

practiceMakesPerfect :: CardDef
practiceMakesPerfect =
  (event "06197" "Practice Makes Perfect" 1 Seeker)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = setFromList [Gambit, Tactic]
    , cdFastWindow = Just FastPlayerWindow
    , cdCriteria = Just $ Criteria.DuringSkillTest SkillTestAtYourLocation
    }

extensiveResearch1 :: CardDef
extensiveResearch1 =
  (event "06198" "Extensive Research" 10 Seeker)
    { cdSkills = [#intellect, #intellect]
    , cdCardTraits = singleton Insight
    , cdCardInHandEffects = True
    , cdCriteria = Just canDiscoverCluesAtYourLocation
    , cdLevel = Just 1
    }

spectralRazor :: CardDef
spectralRazor =
  (event "06201" "Spectral Razor" 2 Mystic)
    { cdSkills = [#willpower, #combat]
    , cdCardTraits = singleton Spell
    , cdActions = [#fight]
    , cdCriteria = Just $ exists $ oneOf [CanFightEnemy ThisCard, CanEngageEnemy ThisCard]
    , cdOverrideActionPlayableIfCriteriaMet = True
    }

wordOfCommand2 :: CardDef
wordOfCommand2 =
  (event "06202" "Word of Command" 2 Mystic)
    { cdCardTraits = setFromList [Spell]
    , cdLevel = Just 2
    , cdCriteria = can.manipulate.deck You
    }

lucidDreaming2 :: CardDef
lucidDreaming2 =
  (event "06205" "Lucid Dreaming" 1 Neutral)
    { cdCardTraits = setFromList [Spell]
    , cdLevel = Just 2
    , cdCriteria =
        Just
          $ youExist can.manipulate.deck
          <> exists (oneOf [InPlayAreaOf You, InHandOf (You <> can.reveal.cards) <> NotThisCard])
    }

heroicRescue2 :: CardDef
heroicRescue2 =
  (event "06234" "Heroic Rescue" 0 Guardian)
    { cdSkills = [#willpower, #combat, #agility]
    , cdCardTraits = setFromList [Spirit, Tactic]
    , cdFastWindow =
        Just
          $ EnemyWouldAttack
            #when
            ( affectsOthers
                $ NotYou
                <> oneOf
                  [InvestigatorAt YourLocation, InvestigatorAt (CanMoveToLocation You ThisCard ConnectedLocation)]
            )
            AnyEnemyAttack
            NonEliteEnemy
    , cdLevel = Just 2
    }

aGlimmerOfHope :: CardDef
aGlimmerOfHope =
  (event "06245" "A Glimmer of Hope" 1 Survivor)
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Blessed, Fortune]
    , cdKeywords = singleton Keyword.Myriad
    , cdCriteria = Just Criteria.InYourDiscard
    , cdPlayableFromDiscard = True
    }

nothingLeftToLose3 :: CardDef
nothingLeftToLose3 =
  (event "06284" "Nothing Left to Lose" 0 Survivor)
    { cdSkills = [#wild]
    , cdCardTraits = singleton Spirit
    , cdCriteria =
        Just
          $ exists
            (You <> oneOf [InvestigatorWithResources (lessThan 5), HandWith (LengthIs $ lessThan 5)])
    , cdLevel = Just 3
    }
