module Arkham.Event.Cards.TheForgottenAge where

import Arkham.Asset.Uses qualified as Uses
import Arkham.Criteria qualified as Criteria
import Arkham.Event.Cards.Import

smuggledGoods :: CardDef
smuggledGoods =
  signature "04003"
    $ (event "04010" "Smuggled Goods" 0 Neutral)
      { cdSkills = [#wild]
      , cdCardTraits = setFromList [Supply, Illicit]
      , cdCriteria = Just $ Criteria.Negate $ exists $ EnemyAt YourLocation <> ReadyEnemy
      }

trusted :: CardDef
trusted =
  (event "04019" "Trusted" 1 Guardian)
    { cdSkills = [#willpower]
    , cdCardTraits = singleton Upgrade
    , cdFastWindow = Just $ DuringTurn You
    , cdCriteria = Just $ exists $ AssetControlledBy You <> #ally
    }

reliable1 :: CardDef
reliable1 =
  (event "04020" "Reliable" 1 Guardian)
    { cdSkills = [#intellect]
    , cdCardTraits = singleton Upgrade
    , cdFastWindow = Just $ DuringTurn You
    , cdCriteria = Just $ exists $ AssetControlledBy You <> #item
    , cdLevel = Just 1
    }

unearthTheAncients :: CardDef
unearthTheAncients =
  (event "04024" "Unearth the Ancients" 1 Seeker)
    { cdSkills = [#intellect, #intellect]
    , cdCardTraits = singleton Insight
    , cdActions = [#investigate]
    , cdCriteria = Just $ Criteria.ExtendedCardExists $ InHandOf You <> basic (#seeker <> #asset)
    }

eavesdrop :: CardDef
eavesdrop =
  (event "04027" "Eavesdrop" 1 Rogue)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = setFromList [Insight, Trick]
    , cdCriteria = Just $ exists $ UnengagedEnemy <> EnemyAt YourLocation <> EnemyWithEvade
    }

youHandleThisOne :: CardDef
youHandleThisOne =
  (event "04028" "\"You handle this one!\"" 0 Rogue)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = singleton Trick
    , cdCriteria = Just (exists $ affectsOthers NotYou)
    , cdFastWindow = Just $ DrawCard #when You (basic $ NonPeril <> IsEncounterCard) EncounterDeck
    }

darkProphecy :: CardDef
darkProphecy =
  (event "04032" "Dark Prophecy" 1 Mystic)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = singleton Augury
    , cdFastWindow = Just $ WouldRevealChaosToken #when You
    , cdAlternateCardCodes = ["60417"]
    }

improvisedWeapon :: CardDef
improvisedWeapon =
  (event "04033" "Improvised Weapon" 1 Survivor)
    { cdCardTraits = setFromList [Tactic, Improvised]
    , cdActions = [#fight]
    , cdPlayableFromDiscard = True
    }

dumbLuck :: CardDef
dumbLuck =
  (event "04034" "Dumb Luck" 2 Survivor)
    { cdSkills = [#agility, #agility]
    , cdCardTraits = singleton Fortune
    , cdCriteria = Just $ exists $ You <> can.target.encounterDeck
    , cdFastWindow =
        Just
          $ SkillTestResult #after You (WhileEvadingAnEnemy NonEliteEnemy)
          $ FailureResult
          $ lessThan 3
    , cdAlternateCardCodes = ["60514"]
    }

darkPact :: CardDef
darkPact =
  (event "04038" "Dark Pact" 2 Neutral)
    { cdCardTraits = singleton Pact
    , cdCardSubType = Just BasicWeakness
    , cdLevel = Nothing
    , cdCardInHandEffects = True
    , cdDeckRestrictions = [CampaignModeOnly]
    }

sceneOfTheCrime :: CardDef
sceneOfTheCrime =
  (event "04103" "Scene of the Crime" 2 Guardian)
    { cdSkills = [#combat, #intellect]
    , cdCardTraits = setFromList [Insight, Bold]
    , cdAttackOfOpportunityModifiers = [DoesNotProvokeAttacksOfOpportunity]
    , cdCriteria =
        Just $ Criteria.Criteria [Criteria.FirstAction, canDiscoverCluesAtYourLocation]
    }

marksmanship1 :: CardDef
marksmanship1 =
  (event "04104" "Marksmanship" 2 Guardian)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = singleton Tactic
    , cdFastWindow =
        Just
          $ ActivateAbility #when You
          $ AbilityIsAction #fight
          <> AssetAbility (oneOf [AssetWithTrait Firearm, AssetWithTrait Ranged])
    , cdCardInHandEffects = True
    , cdLevel = Just 1
    }

persuasion :: CardDef
persuasion =
  (event "04105" "Persuasion" 2 Seeker)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = setFromList [Insight, Trick]
    , cdCriteria =
        Just
          $ Criteria.TabooCriteria
            TabooList21
            ( exists (NonWeaknessEnemy <> EnemyAt YourLocation <> CanParleyEnemy You)
                <> exists (You <> can.target.encounterDeck)
            )
            ( exists (NonWeaknessEnemy <> EnemyWithTrait Humanoid <> EnemyAt YourLocation <> CanParleyEnemy You)
                <> exists (You <> can.target.encounterDeck)
            )
    , cdActions = [#parley]
    }

counterspell2 :: CardDef
counterspell2 =
  (event "04110" "Counterspell" 2 Mystic)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = setFromList [Spell, Blessed]
    , cdFastWindow =
        Just
          $ RevealChaosToken #when You
          $ ChaosTokenMatchesAny
          $ map ChaosTokenFaceIs [#skull, #cultist, #tablet, #elderthing]
    , cdLevel = Just 2
    }

perseverance :: CardDef
perseverance =
  (event "04111" "Perseverance" 2 Survivor)
    { cdSkills = [#willpower, #willpower]
    , cdCardTraits = singleton Spirit
    , cdFastWindow =
        Just
          $ InvestigatorWouldBeDefeated
            #when
            (BySource (SourceIsCancelable AnySource) <> ByAnyOf [ByHorror, ByDamage])
            You
    }

secondWind :: CardDef
secondWind =
  (event "04149" "Second Wind" 1 Guardian)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Spirit, Bold]
    , cdCriteria =
        Just $ Criteria.FirstAction <> exists (HealableInvestigator ThisCard #damage You)
    }

truthFromFiction :: CardDef
truthFromFiction =
  (event "04152" "Truth from Fiction" 2 Seeker)
    { cdSkills = [#intellect, #intellect]
    , cdCardTraits = singleton Insight
    , cdCriteria =
        Just
          $ Criteria.ClueOnLocation
          <> exists (AssetControlledBy You <> AssetCanHaveUses Uses.Secret)
    }

customAmmunition3 :: CardDef
customAmmunition3 =
  (event "04193" "Custom Ammunition" 3 Guardian)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Upgrade, Supply, Blessed]
    , cdCriteria =
        Just
          $ exists
          $ AssetControlledBy (affectsOthers $ InvestigatorAt YourLocation)
          <> #firearm
          <> NotAsset (AssetWithAttachedEvent $ EventCardMatch $ cardIs customAmmunition3)
    , cdFastWindow = Just $ DuringTurn You
    , cdLevel = Just 3
    }

exposeWeakness3 :: CardDef
exposeWeakness3 =
  (event "04195" "Expose Weakness" 0 Seeker)
    { cdSkills = [#intellect, #combat, #wild]
    , cdCardTraits = singleton Insight
    , cdFastWindow = Just FastPlayerWindow
    , cdCriteria = Just $ exists $ EnemyAt YourLocation <> EnemyWithFight
    , cdLevel = Just 3
    }

premonition :: CardDef
premonition =
  (event "04199" "Premonition" 0 Mystic)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = singleton Augury
    , cdFastWindow = Just FastPlayerWindow
    }

liveAndLearn :: CardDef
liveAndLearn =
  (event "04200" "Live and Learn" 0 Survivor)
    { cdSkills = [#wild]
    , cdCardTraits = singleton Spirit
    , cdFastWindow = Just $ SkillTestEnded #after You SkillTestWasFailed
    , cdAlternateCardCodes = ["60516"]
    }

againstAllOdds2 :: CardDef
againstAllOdds2 =
  (event "04202" "Against All Odds" 2 Survivor)
    { cdCardTraits = singleton Spirit
    , cdSkills = [#willpower, #combat, #agility]
    , cdFastWindow = Just $ InitiatedSkillTest #when You AnySkillType GreaterThanBaseValue #any
    , cdLevel = Just 2
    }

slipAway :: CardDef
slipAway =
  (event "04232" "Slip Away" 2 Rogue)
    { cdCardTraits = singleton Trick
    , cdSkills = [#intellect, #agility]
    , cdActions = [#evade]
    , cdAlternateCardCodes = ["60314"]
    }

payDay1 :: CardDef
payDay1 =
  (event "04233" "Pay Day" 0 Rogue)
    { cdCardTraits = setFromList [Illicit, Fated]
    , cdLevel = Just 1
    }

sacrifice1 :: CardDef
sacrifice1 =
  (event "04234" "Sacrifice" 0 Mystic)
    { cdSkills = [#willpower]
    , cdCardTraits = singleton Ritual
    , cdCriteria = Just $ exists $ #mystic <> AssetControlledBy You <> DiscardableAsset
    , cdLevel = Just 1
    }

bloodEclipse3 :: CardDef
bloodEclipse3 =
  (event "04266" "Blood Eclipse" 1 Guardian)
    { cdSkills = [#willpower, #combat]
    , cdCardTraits = setFromList [Spell, Spirit]
    , cdActions = [#fight]
    , cdAdditionalCost = Just $ UpTo (Fixed 3) $ InvestigatorDamageCost ThisCard You DamageAny 1
    , cdLevel = Just 3
    }

coupDeGrace :: CardDef
coupDeGrace =
  (event "04269" "Coup de Grâce" 2 Rogue)
    { cdSkills = [#combat, #combat]
    , cdCardTraits = setFromList [Tactic, Fated]
    , cdAttackOfOpportunityModifiers = [DoesNotProvokeAttacksOfOpportunity]
    , cdCriteria =
        Just
          $ exists (EnemyAt YourLocation <> EnemyCanBeDamagedBySource ThisCard)
          <> Criteria.CanDealDamage
    }

wingingIt :: CardDef
wingingIt =
  (event "04272" "Winging It" 1 Survivor)
    { cdCardTraits = setFromList [Tactic, Improvised]
    , cdActions = [#investigate]
    , cdPlayableFromDiscard = True
    }

vantagePoint :: CardDef
vantagePoint =
  (event "04306" "Vantage Point" 1 Seeker)
    { cdCardTraits = singleton Insight
    , cdSkills = [#intellect, #agility]
    , cdCriteria = Just $ Criteria.DuringTurn Anyone
    , cdFastWindow =
        Just
          $ OrWindowMatcher
            [ PutLocationIntoPlay #after Anyone Anywhere
            , RevealLocation #after Anyone Anywhere
            ]
    }

impromptuBarrier :: CardDef
impromptuBarrier =
  (event "04312" "Impromptu Barrier" 1 Survivor)
    { cdCardTraits = setFromList [Tactic, Improvised]
    , cdActions = [#evade]
    , cdPlayableFromDiscard = True
    }

alterFate3 :: CardDef
alterFate3 =
  (event "04313" "Alter Fate" 1 Survivor)
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Spell, Blessed]
    , cdFastWindow = Just FastPlayerWindow
    , cdCriteria =
        Just
          $ exists
          $ NotTreachery (TreacheryOnEnemy EliteEnemy)
          <> TreacheryIsNonWeakness
    , cdLevel = Just 3
    }
