module Arkham.Event.Cards.TheInnsmouthConspiracy where

import Arkham.Asset.Uses qualified as Uses
import Arkham.Criteria qualified as Criteria
import Arkham.Event.Cards.Import

obscureStudies :: CardDef
obscureStudies =
  signature "07002"
    $ (event "07008" "Obscure Studies" 0 Neutral)
      { cdSkills = [#wild, #wild, #wild]
      , cdCardTraits = singleton Insight
      , cdFastWindow = Just $ InitiatedSkillTest #when You #any #any #any
      }

inTheShadows :: CardDef
inTheShadows =
  signature "07003"
    $ (event "07010" "In the Shadows" 0 Neutral)
      { cdSkills = [#agility, #agility, #wild, #wild]
      , cdCardTraits = singleton Tactic
      , cdFastWindow = Just (TurnBegins #after You)
      }

handOfFate :: CardDef
handOfFate =
  (event "07020" "Hand of Fate" 3 Guardian)
    { cdSkills = [#willpower, #combat]
    , cdCardTraits = setFromList [Spell, Blessed]
    , cdFastWindow =
        Just
          $ EnemyAttacks
            #when
            (affectsOthers $ InvestigatorAt YourLocation)
            (CancelableEnemyAttack AnyEnemyAttack)
            AnyEnemy
    }

deepKnowledge :: CardDef
deepKnowledge =
  (event "07023" "Deep Knowledge" 0 Seeker)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = setFromList [Insight, Cursed]
    , cdAdditionalCost = Just $ AddCurseTokenCost 2
    , cdCriteria =
        Just $ exists $ affectsOthers $ can.draw.cards FromPlayerCardEffect <> InvestigatorAt YourLocation
    }

faustianBargain :: CardDef
faustianBargain =
  (event "07028" "Faustian Bargain" 0 Rogue)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = setFromList [Pact, Cursed]
    , cdAdditionalCost = Just $ AddCurseTokenCost 2
    , cdCriteria =
        Just $ exists $ affectsOthers $ can.gain.resources <> InvestigatorAt YourLocation
    }

tidesOfFate :: CardDef
tidesOfFate =
  (event "07030" "Tides of Fate" 1 Mystic)
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Spell, Blessed]
    , cdFastWindow = Just $ oneOf [FastPlayerWindow, RoundBegins #when]
    , cdCriteria = Just $ Criteria.ChaosTokenCountIs #bless (atLeast 1)
    }

wardOfRadiance :: CardDef
wardOfRadiance =
  (event "07031" "Ward of Radiance" 0 Mystic)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = setFromList [Insight, Blessed]
    , cdFastWindow =
        Just
          $ DrawCard
            #when
            (affectsOthers $ InvestigatorAt YourLocation)
            (CanCancelRevelationEffect $ basic NonWeaknessTreachery)
            EncounterDeck
    }

keepFaith :: CardDef
keepFaith =
  (event "07034" "Keep Faith" 2 Survivor)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Fortune, Blessed]
    , cdFastWindow = Just FastPlayerWindow
    , cdCriteria = Just Criteria.HasRemainingBlessTokens
    }

temptFate :: CardDef
temptFate =
  (event "07037" "Tempt Fate" 0 Neutral)
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Fortune, Blessed, Cursed]
    , cdFastWindow = Just FastPlayerWindow
    , cdCriteria =
        Just
          $ oneOf
            [ Criteria.HasRemainingBlessTokens
            , Criteria.HasRemainingCurseTokens
            , can.draw.cards FromPlayerCardEffect You
            ]
    }

righteousHunt1 :: CardDef
righteousHunt1 =
  (event "07109" "Righteous Hunt" 1 Guardian)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Tactic, Blessed]
    , cdActions = [#engage]
    , cdLevel = Just 1
    , cdOverrideActionPlayableIfCriteriaMet = True
    , cdCriteria =
        Just
          $ exists
          $ CanEngageEnemyWithOverride
          $ Criteria.CriteriaOverride
          $ exists
          $ EnemyAt
          $ LocationWithAccessiblePath ThisCard 2 You Anywhere
    }

stirringUpTrouble1 :: CardDef
stirringUpTrouble1 =
  (event "07112" "Stirring Up Trouble" 0 Seeker)
    { cdSkills = [#combat, #intellect]
    , cdCardTraits = setFromList [Insight, Cursed]
    , cdAdditionalCost = Just AddCurseTokensEqualToShroudCost
    , cdCriteria = Just canDiscoverCluesAtYourLocation
    , cdLevel = Just 1
    }

breakingAndEntering :: CardDef
breakingAndEntering =
  (event "07114" "Breaking and Entering" 2 Rogue)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = setFromList [Trick]
    , cdActions = [#investigate]
    , cdAttackOfOpportunityModifiers = [DoesNotProvokeAttacksOfOpportunity]
    }

radiantSmite1 :: CardDef
radiantSmite1 =
  (event "07153" "Radiant Smite" 1 Guardian)
    { cdSkills = [#willpower, #combat]
    , cdCardTraits = setFromList [Spirit, Spell, Blessed]
    , cdActions = [#fight]
    , cdLevel = Just 1
    }

theTruthBeckons :: CardDef
theTruthBeckons =
  (event "07154" "The Truth Beckons" 1 Seeker)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = setFromList [Insight]
    , cdCriteria =
        Just
          $ notExists EnemyEngagedWithYou
          <> exists (CanMoveCloserToLocation ThisCard You UnrevealedLocation)
    }

gazeOfOuraxsh2 :: CardDef
gazeOfOuraxsh2 =
  (event "07155" "Gaze of Ouraxsh" 2 Seeker)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Spell, Cursed]
    , cdAttackOfOpportunityModifiers = [DoesNotProvokeAttacksOfOpportunity]
    , cdLevel = Just 2
    }

underSurveillance1 :: CardDef
underSurveillance1 =
  (event "07157" "Under Surveillance" 3 Rogue)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = setFromList [Tactic, Trap]
    , cdCriteria = Just $ Criteria.Negate $ exists $ "Under Surveillance" <> AssetAt YourLocation
    , cdLevel = Just 1
    }

butterflyEffect1 :: CardDef
butterflyEffect1 =
  (event "07160" "Butterfly Effect" 0 Survivor)
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Paradox, Blessed, Cursed]
    , cdFastWindow = Just $ RevealChaosToken #when (affectsOthers Anyone) IsSymbol
    , cdCriteria =
        Just
          $ Criteria.DuringSkillTest SkillTestAtYourLocation
          <> oneOf
            [ exists (CardIsCommittedBy (affectsOthers $ InvestigatorAt YourLocation))
            , exists (affectsOthers $ InvestigatorAt YourLocation <> InvestigatorWithCommittableCard)
            ]
    , cdLevel = Just 1
    }

thirdTimesACharm2 :: CardDef
thirdTimesACharm2 =
  (event "07161" "Third Time's a Charm" 1 Survivor)
    { cdSkills = [#willpower, #combat, #agility]
    , cdCardTraits = setFromList [Spirit]
    , cdFastWindow = Just $ InitiatedSkillTest #when (affectsOthers Anyone) #any #any #any
    , cdCriteria = Just $ Criteria.DuringSkillTest SkillTestAtYourLocation
    , cdLevel = Just 2
    }

manipulateDestiny2 :: CardDef
manipulateDestiny2 =
  (event "07162" "Manipulate Destiny" 1 Neutral)
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Spell]
    , cdAttackOfOpportunityModifiers = [DoesNotProvokeAttacksOfOpportunity]
    , cdLevel = Just 2
    }

riastrad1 :: CardDef
riastrad1 =
  (event "07193" "Ríastrad" 0 Rogue)
    { cdSkills = [#combat, #combat]
    , cdCardTraits = setFromList [Spell, Spirit, Cursed]
    , cdActions = [#fight]
    , cdLevel = Just 1
    }

harmonyRestored2 :: CardDef
harmonyRestored2 =
  (event "07230" "Harmony Restored" 3 Survivor)
    { cdSkills = [#willpower, #willpower]
    , cdCardTraits = setFromList [Fortune, Blessed]
    , cdCriteria =
        Just
          $ Criteria.ChaosTokenCountIs #bless (atLeast 1)
          <> Criteria.ChaosTokenCountIs #curse (atLeast 1)
    , cdLevel = Just 2
    }

enchantWeapon3 :: CardDef
enchantWeapon3 =
  (event "07261" "Enchant Weapon" 3 Guardian)
    { cdSkills = [#willpower, #willpower, #combat]
    , cdCardTraits = setFromList [Spell, Upgrade]
    , cdCriteria =
        Just
          $ exists
          $ AssetControlledBy (affectsOthers $ InvestigatorAt YourLocation)
          <> AssetWithTrait Weapon
          <> not_ (AssetWithAttachedEvent $ EventIs "07261")
    , cdLevel = Just 3
    }

theStygianEye3 :: CardDef
theStygianEye3 =
  (event "07263" "The Stygian Eye" 10 Seeker)
    { cdSkills = [#willpower, #willpower, #willpower]
    , cdCardTraits = setFromList [Insight, Cursed]
    , cdFastWindow = Just $ DuringTurn You
    , cdCardInHandEffects = True
    , cdLevel = Just 3
    }

aWatchfulPeace3 :: CardDef
aWatchfulPeace3 =
  (event "07269" "A Watchful Peace" 1 Survivor)
    { cdSkills = [#willpower, #willpower]
    , cdCardTraits = setFromList [Spirit, Blessed]
    , cdFastWindow = Just $ MythosStep WhenAllDrawEncounterCard
    , cdCriteria = Just $ exists $ You <> can.target.encounterDeck
    , cdAdditionalCost = Just $ ReturnChaosTokensToPoolCost 5 #bless
    , cdLevel = Just 3
    }

hallow3 :: CardDef
hallow3 =
  (event "07301" "Hallow" 3 Guardian)
    { cdSkills = [#willpower, #wild]
    , cdCardTraits = setFromList [Spell, Blessed]
    , cdCriteria = Just Criteria.CardWithDoomExists
    , cdAdditionalCost = Just $ ReturnChaosTokensToPoolCost 10 (IncludeSealed #bless)
    , cdLevel = Just 3
    }

riteOfEquilibrium5 :: CardDef
riteOfEquilibrium5 =
  (event "07308" "Rite of Equilibrium" 0 Mystic)
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Spell, Blessed, Cursed]
    , cdCriteria =
        Just
          $ oneOf
            [ Criteria.HasRemainingCurseTokens <> Criteria.HasRemainingBlessTokens
            , Criteria.ChaosTokenCountIs #curse (atLeast 1)
                <> Criteria.ChaosTokenCountIs #bless (atLeast 1)
                <> oneOf
                  [ exists (HealableAsset ThisCard #horror $ AssetAt YourLocation)
                  , exists (HealableInvestigator ThisCard #horror $ InvestigatorAt YourLocation)
                  ]
            ]
    , cdLevel = Just 5
    }

shrineOfTheMoirai3 :: CardDef
shrineOfTheMoirai3 =
  (event "07310" "Shrine of the Moirai" 1 Survivor)
    { cdSkills = [#willpower, #intellect, #agility]
    , cdCardTraits = setFromList [Fortune, Blessed, Cursed]
    , cdCriteria = Just $ exists (LocationWithInvestigator You)
    , cdLevel = Just 3
    , cdUnique = True
    , cdUses = Uses.Uses Uses.Offering (Fixed 3)
    }
