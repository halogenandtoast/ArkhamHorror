module Arkham.Event.Cards.TheDrownedCity where

import Arkham.Capability
import Arkham.Criteria qualified as Criteria
import Arkham.Event.Cards.Import
import Arkham.Keyword qualified as Keyword

psychicSensitivity :: CardDef
psychicSensitivity =
  signature "11014"
    $ (event "11015" "Psychic Sensitivity" 0 Neutral)
      { cdSkills = [#willpower, #willpower, #wild]
      , cdCardTraits = setFromList [Augury, Insight]
      , cdFastWindow =
          Just
            $ DrawCard
              #when
              (at_ Anywhere)
              ( CanCancelAllEffects
                  $ basic IsEncounterCard
                  <> CardSharesTitleWith
                    (CardIsBeneathInvestigator $ InvestigatorWithTitle "Gloria Goldberg")
              )
              AnyDeck
      }

primedForAction :: CardDef
primedForAction =
  (event "11023" "Primed for Action" 0 Guardian)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = setFromList [Tactic, Bold]
    , cdCriteria =
        Just
          $ Criteria.FirstAction
          <> Criteria.PlayableCardExistsWithCostReduction (Reduce 2) (InHandOf ForPlay You <> basic #firearm)
    }

readyForAnything :: CardDef
readyForAnything =
  (event "11024" "Ready for Anything" 1 Guardian)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Spirit, Bold]
    , cdCriteria = Just $ Criteria.FirstAction <> can.draw.cards You
    , cdAttackOfOpportunityModifiers = [DoesNotProvokeAttacksOfOpportunity]
    }

huntersMark1 :: CardDef
huntersMark1 =
  (event "11026" "Hunter's Mark" 1 Guardian)
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Spell]
    , cdCriteria = Just $ exists $ EnemyAt YourLocation <> not_ (EnemyWithAttachedEvent $ EventIs "11026")
    , cdFastWindow = Just $ DuringTurn You
    }

quickShot3 :: CardDef
quickShot3 =
  (event "11031" "Quick Shot" 2 Guardian)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Spirit]
    , cdKeywords = setFromList [Keyword.Myriad]
    , cdCriteria = Just $ exists $ EnemyAt YourLocation <> EnemyCanBeDamagedBySource ThisCard
    , cdFastWindow = Just $ DuringTurn You
    , cdCardInHandEffects = True
    }

correlateAllItsContents :: CardDef
correlateAllItsContents =
  (event "11040" "Correlate All Its Contents" 1 Seeker)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = setFromList [Insight]
    , cdActions = [#investigate]
    }

cosmicRevelation1 :: CardDef
cosmicRevelation1 =
  (event "11041" "Cosmic Revelation" 0 Seeker)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = setFromList [Insight, Spell]
    , cdCriteria = Just $ exists $ investigator_ can.reveal.cards
    }

workingAHunch2 :: CardDef
workingAHunch2 =
  (event "11045" "Working a Hunch" 2 Seeker)
    { cdSkills = [#intellect, #intellect]
    , cdCardTraits = setFromList [Insight]
    , cdFastWindow = Just $ DuringTurn You
    , cdCriteria = Just $ exists $ You <> InvestigatorCanDiscoverCluesAt RevealedLocation
    , cdLevel = Just 2
    }

wheresTheParty :: CardDef
wheresTheParty =
  (event "11053" "\"Where's the party?\"" 0 Rogue)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = setFromList [Trick, Improvised]
    , cdActions = [#parley]
    , cdCriteria = Just $ can.target.encounterDeck You
    }

youveHadWorse :: CardDef
youveHadWorse =
  (event "11054" "\"You've had worse…\"" 0 Rogue)
    { cdSkills = [#willpower, #agility, #agility]
    , cdCardTraits = setFromList [Favor]
    , cdFastWindow =
        Just
          $ DealtDamageOrHorror
            #when
            (SourceIsCancelable AnySource)
            (affectsOthers $ at_ (orConnected YourLocation) <> can.spend.resources)
    }

bumsRush :: CardDef
bumsRush =
  (event "11055" "Bum's Rush" 2 Rogue)
    { cdSkills = [#intellect, #combat]
    , cdCardTraits = setFromList [Tactic, Trick]
    , cdActions = [#evade]
    }

intimidation :: CardDef
intimidation =
  (event "11056" "Intimidation" 3 Rogue)
    { cdSkills = [#willpower, #combat]
    , cdCardTraits = setFromList [Tactic]
    , cdActions = [#parley]
    , cdCriteria = Just $ exists $ EnemyAt YourLocation <> EnemyWithRemainingHealth (atLeast 1)
    }

doppelganger1 :: CardDef
doppelganger1 =
  (event "11058" "Doppelgänger" 2 Rogue)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = setFromList [Spell, Trick]
    , cdFastWindow = Just $ DuringTurn You
    , cdKeywords = setFromList [Keyword.Myriad]
    , cdCriteria = Just $ exists $ YourLocation <> not_ (LocationWithAttachedEvent $ EventIs "11058")
    , cdLevel = Just 1
    }

youveHadWorse4 :: CardDef
youveHadWorse4 =
  (event "11063" "\"You've had worse…\"" 0 Rogue)
    { cdSkills = [#willpower, #agility, #agility]
    , cdCardTraits = setFromList [Favor]
    , cdFastWindow =
        Just
          $ DealtDamageOrHorror
            #when
            (SourceIsCancelable AnySource)
            (affectsOthers $ at_ (orConnected YourLocation) <> InvestigatorWithResources (atLeast 1))
    }

spectralShield :: CardDef
spectralShield =
  (event "11071" "Spectral Shield" 1 Mystic)
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Spell, Spirit]
    , cdFastWindow = Just $ DuringTurn You
    }

whispersOfDoom :: CardDef
whispersOfDoom =
  (event "11072" "Whispers of Doom" 3 Mystic)
    { cdSkills = [#willpower, #combat]
    , cdCardTraits = setFromList [Spell, Cursed]
    , cdActions = [#parley]
    , cdCriteria = Just $ exists $ at_ YourLocation <> NonWeaknessEnemy
    }

spiritualEcho2 :: CardDef
spiritualEcho2 =
  (event "11075" "Spiritual Echo" 1 Mystic)
    { cdSkills = [#willpower, #combat]
    , cdCardTraits = setFromList [Ritual]
    , cdCriteria = Just $ exists $ YourLocation <> not_ (LocationWithAttachedEvent $ EventIs "11075")
    , cdLevel = Just 2
    }

transfiguration2 :: CardDef
transfiguration2 =
  (event "11076" "Transfiguration" 0 Mystic)
    { cdSkills = [#willpower, #intellect, #wild]
    , cdCardTraits = setFromList [Ritual]
    , cdLevel = Just 2
    , cdLimits = [MaxPerGame 1]
    }

deliverance3 :: CardDef
deliverance3 =
  (event "11079" "Deliverance" 0 Mystic)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Spirit]
    , cdLevel = Just 3
    , cdFastWindow = Just $ MythosStep WhenAllDrawEncounterCard
    , cdCriteria = Just $ exists $ You <> can.target.encounterDeck
    , cdLimits = [MaxPerGame 2]
    }

catch :: CardDef
catch =
  (event "11086" "\"Catch!\"" 0 Survivor)
    { cdSkills = [#agility, #agility]
    , cdCardTraits = setFromList [Tactic, Trick]
    , cdActions = [#evade]
    , cdAdditionalCost =
        Just $ DiscardFromCost 1 (FromHandOf You <> FromPlayAreaOf You) (#item <> CardFillsSlot #hand)
    }

unconventionalMethod :: CardDef
unconventionalMethod =
  (event "11087" "Unconventional Method" 0 Survivor)
    { cdSkills = [#intellect, #intellect]
    , cdCardTraits = setFromList [Insight, Tactic]
    , cdActions = [#investigate]
    , cdAdditionalCost =
        Just $ DiscardFromCost 1 (FromHandOf You <> FromPlayAreaOf You) (#item <> CardFillsSlot #hand)
    }

aGlimmerOfHope2 :: CardDef
aGlimmerOfHope2 =
  (event "11090" "A Glimmer of Hope" 1 Survivor)
    { cdSkills = [#willpower, #agility, #wild]
    , cdCardTraits = setFromList [Blessed, Fortune]
    , cdKeywords = singleton Keyword.Myriad
    , cdCriteria = Just Criteria.InYourDiscard
    , cdPlayableFromDiscard = True
    }

goodWeather2 :: CardDef
goodWeather2 =
  (event "11091" "Good Weather" 1 Survivor)
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Blessed, Fortune]
    , cdKeywords = singleton Keyword.Myriad
    , cdFastWindow = Just $ PhaseBegins #after #investigation
    , cdLimits = [LimitInPlay 1]
    }

improvisedWeapon2 :: CardDef
improvisedWeapon2 =
  (event "11092" "Improvised Weapon" 1 Survivor)
    { cdCardTraits = setFromList [Tactic, Improvised]
    , cdActions = [#fight]
    , cdPlayableFromDiscard = True
    , cdLevel = Just 2
    }

shortRest :: CardDef
shortRest =
  (event "11096" "Short Rest" 1 Neutral)
    { cdSkills = [#willpower]
    , cdKeywords = setFromList [Keyword.Myriad]
    , cdCardInHandEffects = True
    , cdCardTraits = setFromList [Spirit]
    , cdCriteria =
        Just
          $ oneOf
            [ exists
                $ oneOf
                  [ HealableInvestigator ThisCard kind (InvestigatorAt YourLocation)
                  | kind <- [#damage, #horror]
                  ]
            , exists
                $ oneOf
                  [ HealableAsset ThisCard kind (AssetAt YourLocation)
                  | kind <- [#damage, #horror]
                  ]
            ]
    }

nameYourPrice2 :: CardDef
nameYourPrice2 =
  (event "11105" "Name Your Price" 20 Neutral)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = setFromList [Favor]
    , cdCriteria = Just $ exists $ EnemyCanBeDamagedBySource ThisCard <> at_ YourLocation
    , cdDeckRestrictions = [OnlyInvestigatorWithTraits [Criminal, Socialite]]
    , cdActions = [#parley]
    }
