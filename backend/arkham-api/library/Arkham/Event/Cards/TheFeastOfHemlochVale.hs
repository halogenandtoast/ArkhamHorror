module Arkham.Event.Cards.TheFeastOfHemlochVale where

import Arkham.Asset.Uses qualified as Uses
import Arkham.Criteria qualified as Criteria
import Arkham.Event.Cards.Import
import Arkham.Keyword qualified as Keyword
import Arkham.Modifier (ModifierType (..))

adHoc :: CardDef
adHoc =
  signature "10001"
    $ (event "10002" "Ad Hoc" 2 Neutral)
      { cdSkills = [#wild, #wild]
      , cdCardTraits = setFromList [Improvised, Upgrade]
      , cdCriteria = Just $ exists $ AssetControlledBy You <> oneOf [#tool, #weapon]
      }

aethericCurrentYuggoth :: CardDef
aethericCurrentYuggoth =
  signature "10004"
    $ (event "10006" ("Aetheric Current" <:> "Yuggoth") 0 Neutral)
      { cdSkills = [#combat]
      , cdKeywords = singleton (Keyword.Bonded 1 "06005")
      , cdCardTraits = setFromList [Science]
      , cdActions = [#fight]
      , cdCriteria = Just $ exists $ AssetIs "10005b"
      }

aethericCurrentYoth :: CardDef
aethericCurrentYoth =
  signature "10004"
    $ (event "10007" ("Aetheric Current" <:> "Yoth") 0 Neutral)
      { cdSkills = [#agility]
      , cdKeywords = singleton (Keyword.Bonded 1 "06005")
      , cdCardTraits = setFromList [Science]
      , cdActions = [#evade]
      , cdCriteria = Just $ exists $ AssetIs "10005b"
      }

beguile :: CardDef
beguile =
  signature "10009"
    $ (event "10010" "Beguile" 2 Neutral)
      { cdSkills = [#wild]
      , cdCardTraits = setFromList [Trick]
      , cdFastWindow = Just $ DuringTurn You
      , cdCriteria = Just $ exists $ NonEliteEnemy <> EnemyAt YourLocation
      , cdTags = ["parley"]
      }

stouthearted :: CardDef
stouthearted =
  signature "10015"
    $ (event "10017" "Stouthearted" 2 Neutral)
      { cdSkills = [#willpower, #combat, #wild]
      , cdCardTraits = setFromList [Spirit]
      , cdCriteria = Just $ youExist $ oneOf [InvestigatorWithAnyDamage, InvestigatorWithAnyHorror]
      , cdFastWindow = Just $ EnemyEngaged #when You (NonEliteEnemy <> EnemyWithHealth)
      }

absolution :: CardDef
absolution =
  (event "10024" "Absolution" 0 Guardian)
    { cdSkills = [#willpower, #willpower]
    , cdCardTraits = setFromList [Spell, Blessed]
    , cdCriteria =
        Just
          $ oneOf
            [ Criteria.HasRemainingBlessTokens
            , oneOf
                [ exists $ HealableInvestigator ThisCard #horror $ InvestigatorAt YourLocation
                , exists $ HealableAsset ThisCard #horror $ AssetAt YourLocation
                ]
            ]
    , cdCost = Just DynamicCost
    }

guidedByFaith :: CardDef
guidedByFaith =
  (event "10025" "Guided by Faith" 2 Guardian)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = setFromList [Spirit, Blessed]
    , cdActions = [#investigate]
    }

holdUp :: CardDef
holdUp =
  (event "10026" "Hold Up" 1 Guardian)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Tactic, Trick]
    , cdActions = [#parley]
    , cdCriteria = Just $ exists (EnemyAt YourLocation) <> exists (InHandOf You <> #item <> #asset)
    }

taskForce :: CardDef
taskForce =
  (event "10027" "Task Force" 2 Guardian)
    { cdSkills = [#intellect, #combat, #agility]
    , cdCardTraits = setFromList [Tactic, Double]
    , cdAdditionalCost = Just (ActionCost 1)
    , cdCriteria =
        Just
          $ oneOf
            [ exists
                ( AssetWithPerformableAbilityBy
                    (affectsOthers $ InvestigatorAt YourLocation)
                    AbilityIsActionAbility
                    [IgnoreActionCost]
                )
            , exists
                ( affectsOthers
                    $ InvestigatorAt YourLocation
                    <> InvestigatorCanMoveTo ThisCard (ConnectedFrom YourLocation)
                )
            , exists
                (YourLocation <> LocationWithDiscoverableCluesBy (affectsOthers $ InvestigatorAt YourLocation))
            ]
    }

tinker :: CardDef
tinker =
  (event "10028" "Tinker" 1 Guardian)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = setFromList [Insight, Upgrade]
    , cdFastWindow = Just $ DuringTurn You
    , cdCriteria =
        Just (exists $ #tool <> AssetInPlayAreaOf You <> not_ (AssetWithAttachedEvent $ EventIs "10028"))
    }

handEyeCoordination1 :: CardDef
handEyeCoordination1 =
  (event "10030" "Fine Tuning" 1 Guardian)
    { cdSkills = [#intellect, #combat]
    , cdCardTraits = setFromList [Insight]
    , cdFastWindow = Just $ DuringTurn You
    , cdCriteria =
        Just
          ( exists
              $ AssetControlledBy You
              <> oneOf [#tool, #weapon]
              <> AssetWithPerformableAbility AbilityIsActionAbility [IgnoreActionCost]
          )
    , cdLevel = Just 1
    }

secondWind2 :: CardDef
secondWind2 =
  (event "10032" "Second Wind" 0 Guardian)
    { cdSkills = [#willpower, #wild]
    , cdCardTraits = setFromList [Spirit, Bold]
    , cdCriteria =
        Just
          $ Criteria.FirstAction
          <> oneOf [exists (HealableInvestigator ThisCard #damage You), can.draw.cards You]
    , cdLevel = Just 2
    }

flurryOfBlows5 :: CardDef
flurryOfBlows5 =
  (event "10037" "Flurry of Blows" 2 Guardian)
    { cdSkills = [#combat, #agility, #wild]
    , cdCardTraits = setFromList [Tactic, Double, Fated]
    , cdCriteria =
        Just
          $ exists
            ( AssetControlledBy You
                <> #melee
                <> AssetWithPerformableAbility (AbilityIsAction #fight) [IgnoreActionCost]
            )
    , cdAdditionalCost = Just (ActionCost 1)
    , cdActions = [#fight]
    , cdLevel = Just 5
    }

miracleWish5 :: CardDef
miracleWish5 =
  (event "10038" "Miracle Wish" 0 Guardian)
    { cdSkills = [#willpower, #wild]
    , cdCardTraits = setFromList [Favor, Blessed]
    , cdCriteria = Just $ Criteria.DuringSkillTest SkillTestAtYourLocation
    , cdFastWindow = Just $ RevealChaosToken #after You #bless
    , cdLevel = Just 5
    , cdBondedWith = [(1, "10039")]
    }

uncannyGrowth :: CardDef
uncannyGrowth =
  (event "10045" "Uncanny Growth" 1 Seeker)
    { cdCardTraits = setFromList [Insight, Science]
    , cdActions = [#investigate]
    , cdKeywords =
        setFromList
          [ Keyword.Bonded 1 "10044"
          , Keyword.Bonded 1 "10059"
          , Keyword.Bonded 1 "10060"
          , Keyword.Bonded 1 "10061"
          ]
    }

controlVariable :: CardDef
controlVariable =
  (event "10046" "Control Variable" 1 Seeker)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = setFromList [Insight, Science, Cursed]
    , cdCriteria =
        Just
          $ Criteria.DuringSkillTest SkillTestAtYourLocation
          <> canDiscoverCluesAtYourLocation
    , cdFastWindow = Just $ RevealChaosToken #after Anyone #curse
    }

testingSprint :: CardDef
testingSprint =
  (event "10047" "Testing Spring" 1 Seeker)
    { cdSkills = [#willpower, #intellect, #agility]
    , cdCardTraits = setFromList [Insight, Double]
    , cdAdditionalCost = Just (ActionCost 1)
    , cdActions = [#investigate]
    , cdCriteria =
        Just
          $ exists
          $ InvestigatableLocation
          <> oneOf [YourLocation, ConnectedFrom YourLocation]
    }

thoroughInquiry :: CardDef
thoroughInquiry =
  (event "10048" "Thorough Inquiry" 2 Seeker)
    { cdSkills = [#intellect, #agility, #wild]
    , cdCardTraits = setFromList [Insight, Double]
    , cdAdditionalCost = Just (ActionCost 1)
    , cdCriteria =
        Just
          $ exists
          $ affectsOthers
          $ InvestigatorAt YourLocation
          <> can.draw.cards
    }

throwTheBookAtThem :: CardDef
throwTheBookAtThem =
  (event "10049" "\"Throw the Book at Them!\"" 1 Seeker)
    { cdSkills = [#intellect, #combat]
    , cdCardTraits = setFromList [Gambit, Improvised]
    , cdCriteria = Just $ exists $ AssetControlledBy You <> #tome
    , cdActions = [#fight]
    }

transmogrify :: CardDef
transmogrify =
  (event "10050" "Transmogrify" 1 Seeker)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = setFromList [Gambit, Science]
    , cdActions = [#evade]
    }

fineTuning1 :: CardDef
fineTuning1 =
  (event "10054" "Fine Tuning" 2 Seeker)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = setFromList [Insight, Upgrade]
    , cdCriteria =
        Just
          ( exists
              $ oneOf [#tool, #science]
              <> AssetControlledBy You
              <> not_ (AssetWithAttachedEvent $ EventIs "10054")
          )
    , cdLevel = Just 1
    }

confound3 :: CardDef
confound3 =
  (event "10057" "Confound" 2 Seeker)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = setFromList [Insight, Trick]
    , cdActions = [#parley]
    , cdCriteria = Just $ exists $ EnemyAt YourLocation <> EnemyWithEvade
    , cdLevel = Just 3
    }

bankJob :: CardDef
bankJob =
  (event "10069" "Bank Job" 2 Rogue)
    { cdSkills = [#intellect, #combat, #agility]
    , cdCardTraits = setFromList [Gambit, Double, Illicit]
    , cdCriteria = Just (exists $ affectsOthers $ InvestigatorAt YourLocation <> can.gain.resources)
    , cdAdditionalCost = Just (ActionCost 1)
    }

falseSurrender :: CardDef
falseSurrender =
  (event "10070" "False Surrender" 1 Rogue)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = setFromList [Tactic, Trick]
    , cdActions = [#parley]
    , cdCriteria =
        Just
          $ exists (EnemyAt YourLocation <> CanParleyEnemy You)
          <> exists (PlayableCardWithCostReduction NoAction 1 $ InHandOf You <> basic (#asset <> #weapon))
    }

grift :: CardDef
grift =
  (event "10071" "Grift" 0 Rogue)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = setFromList [Trick, Illicit]
    , cdActions = [#parley]
    , cdCriteria = Just $ exists (EnemyAt YourLocation <> CanParleyEnemy You)
    }

illPayYouBack :: CardDef
illPayYouBack =
  (event "10072" "\"I'll Pay You Back!\"" 0 Rogue)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = setFromList [Gambit, Trick]
    , cdFastWindow = Just $ DuringTurn You
    , cdCriteria = Just $ exists $ affectsOthers $ InvestigatorAt YourLocation <> not_ You
    }

stirThePot :: CardDef
stirThePot =
  (event "10073" "Stir the Pot" 3 Rogue)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = setFromList [Trick, Gambit]
    , cdActions = [#parley]
    , cdCriteria = Just $ exists $ EnemyAt YourLocation <> CanParleyEnemy You
    }

vamp :: CardDef
vamp =
  (event "10074" "Vamp" 1 Rogue)
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Trick]
    , cdActions = [#parley]
    , cdCriteria = Just $ exists (EnemyAt YourLocation <> CanParleyEnemy You)
    }

snitch2 :: CardDef
snitch2 =
  (event "10078" "Snitch" 1 Rogue)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = setFromList [Favor, Trick]
    , cdFastWindow = Just $ SkillTestResult #after You #parley #success
    , cdCriteria =
        Just
          $ exists (LocationWithDiscoverableCluesBy You <> oneOf [YourLocation, ConnectedFrom YourLocation])
    , cdTags = ["parley"]
    }

dirtyDeeds3 :: CardDef
dirtyDeeds3 =
  (event "10080" "Dirty Deeds" 1 Rogue)
    { cdSkills = [#combat, #agility, #wild]
    , cdCardTraits = setFromList [Favor, Double, Illicit]
    , cdCriteria = can.search.deck
    , cdAdditionalCost = Just (ActionCost 1)
    , cdLevel = Just 3
    , cdAttackOfOpportunityModifiers = [DoesNotProvokeAttacksOfOpportunity]
    }

vamp3 :: CardDef
vamp3 =
  (event "10081" "Vamp" 1 Rogue)
    { cdSkills = [#wild, #wild]
    , cdCardTraits = setFromList [Trick]
    , cdActions = [#parley]
    , cdCriteria = Just $ exists (EnemyAt YourLocation <> CanParleyEnemy You)
    , cdLevel = Just 3
    }

stirThePot5 :: CardDef
stirThePot5 =
  (event "10083" "Stir the Pot" 3 Rogue)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = setFromList [Trick, Gambit]
    , cdActions = [#parley]
    , cdCriteria = Just $ exists $ EnemyAt YourLocation <> CanParleyEnemy You
    , cdLevel = Just 5
    }

abyssalRot :: CardDef
abyssalRot =
  (event "10086" "Abyssal Rot" 0 Mystic)
    { cdCardTraits = setFromList [Spell, Rot, Cursed]
    , cdCost = Nothing
    , cdKeywords = setFromList [Keyword.Bonded 1 "10085", Keyword.Bonded 1 "10098"]
    }

aemberRot :: CardDef
aemberRot =
  (event "10087" "Aember Rot" 0 Mystic)
    { cdCardTraits = setFromList [Spell, Rot, Cursed]
    , cdCost = Nothing
    , cdKeywords = setFromList [Keyword.Bonded 1 "10085", Keyword.Bonded 1 "10098"]
    }

putrescentRot :: CardDef
putrescentRot =
  (event "10088" "Putrescent Rot" 0 Mystic)
    { cdCardTraits = setFromList [Spell, Rot, Cursed]
    , cdCost = Nothing
    , cdKeywords = setFromList [Keyword.Bonded 1 "10085", Keyword.Bonded 1 "10098"]
    }

scarletRot :: CardDef
scarletRot =
  (event "10089" "Scarlet Rot" 0 Mystic)
    { cdCardTraits = setFromList [Spell, Rot, Cursed]
    , cdCost = Nothing
    , cdKeywords = setFromList [Keyword.Bonded 1 "10085", Keyword.Bonded 1 "10098"]
    }

virescentRot :: CardDef
virescentRot =
  (event "10090" "Virescent Rot" 0 Mystic)
    { cdCardTraits = setFromList [Spell, Rot, Cursed]
    , cdCost = Nothing
    , cdKeywords = setFromList [Keyword.Bonded 1 "10085", Keyword.Bonded 1 "10098"]
    }

antediluvianHymn :: CardDef
antediluvianHymn =
  (event "10093" "Antediluvian Hymn" 2 Mystic)
    { cdSkills = [#willpower, #intellect, #combat]
    , cdCardTraits = setFromList [Augury, Double]
    , cdAdditionalCost = Just (ActionCost 1)
    , cdCriteria = can.target.encounterDeck
    }

drainEssence :: CardDef
drainEssence =
  (event "10094" "Drain Essence" 2 Mystic)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = setFromList [Spell]
    , cdActions = [#parley]
    , cdCriteria = Just $ exists (EnemyAt YourLocation <> EnemyWithFight)
    }

callTheBeyond2 :: CardDef
callTheBeyond2 =
  (event "10099" "Call the Beyond" 0 Mystic)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = setFromList [Ritual, Cursed]
    , cdAdditionalCost = Just $ AddCurseTokenCost 3
    , cdCriteria =
        Just
          $ exists
            ( AssetControlledBy You
                <> oneOf [AssetWithUseType Uses.Charge, AssetWithUseType Uses.Secret]
                <> oneOf
                  [ AssetNotAtUsesX
                  , AssetWithPerformableAbility
                      (oneOf [AbilityIsActionAbility, AbilityIsFastAbility])
                      [IgnoreActionCost]
                  ]
            )
    }

etherealForm2 :: CardDef
etherealForm2 =
  (event "10100" "Ethereal Form" 2 Mystic)
    { cdSkills = [#willpower, #agility, #wild]
    , cdActions = [#evade]
    , cdCardTraits = setFromList [Spell]
    }

readTheSigns2 :: CardDef
readTheSigns2 =
  (event "10101" "Read the Signs" 2 Mystic)
    { cdSkills = [#willpower, #intellect, #wild]
    , cdActions = [#investigate]
    , cdCardTraits = setFromList [Spell]
    }

spectralRazor2 :: CardDef
spectralRazor2 =
  (event "10102" "Spectral Razor" 2 Mystic)
    { cdSkills = [#willpower, #combat, #wild]
    , cdCardTraits = singleton Spell
    , cdActions = [#fight]
    , cdCriteria = Just $ exists $ oneOf [CanFightEnemy ThisCard, CanEngageEnemy ThisCard]
    , cdOverrideActionPlayableIfCriteriaMet = True
    }

etherealWeaving3 :: CardDef
etherealWeaving3 =
  (event "10103" "Ethereal Weaving" 1 Mystic)
    { cdSkills = [#willpower, #agility, #wild]
    , cdCardTraits = setFromList [Spirit, Double]
    , cdAdditionalCost = Just (ActionCost 1)
    , cdCriteria =
        Just $ Criteria.PlayableCardExistsWithCostReduction (Reduce 1) $ InHandOf You <> #spell <> #event
    }

-- We need to include the token pool because after this skill test the tokens
-- might have been removed.
sealOfTheElders5 :: CardDef
sealOfTheElders5 =
  (event "10105" "Seal of the Elders" 0 Mystic)
    { cdSkills = [#willpower, #combat]
    , cdCardTraits = setFromList [Spell, Pact, Blessed, Cursed]
    , cdFastWindow =
        Just
          $ SkillTestEnded #after Anyone
          $ SkillTestAtYourLocation
          <> oneOf
            [ SkillTestWithRevealedChaosTokenCount 2 (IncludeTokenPool #curse)
            , SkillTestWithRevealedChaosTokenCount 2 (IncludeTokenPool #bless)
            ]
    , cdBondedWith = [(1, "10106"), (1, "10107")]
    }

elaborateDistraction :: CardDef
elaborateDistraction =
  (event "10112" "Elaborate Distraction" 3 Survivor)
    { cdSkills = [#willpower, #combat, #agility]
    , cdCardTraits = setFromList [Trick, Double]
    , cdAdditionalCost = Just (ActionCost 1)
    , cdCriteria =
        Just
          $ exists
          $ EnemyAt (oneOf [YourLocation, ConnectedFrom YourLocation])
          <> oneOf [NonEliteEnemy <> EnemyCanBeEvadedBy ThisCard, EnemyCanBeDamagedBySource ThisCard]
    , cdAttackOfOpportunityModifiers = [DoesNotProvokeAttacksOfOpportunity]
    }

pushedToTheLimit :: CardDef
pushedToTheLimit =
  (event "10113" "Pushed to the Limit" 2 Survivor)
    { cdSkills = [#willpower, #combat]
    , cdCardTraits = setFromList [Tactic, Improvised]
    , cdCriteria =
        Just
          ( exists
              $ oneOf [#tool, #weapon]
              <> #asset
              <> InDiscardOf You
              <> CardWithPerformableAbility AbilityIsActionAbility [IgnoreAllCosts]
          )
    , cdAttackOfOpportunityModifiers = [DoesNotProvokeAttacksOfOpportunity]
    }

stallForTime :: CardDef
stallForTime =
  (event "10114" "Stall for Time" 1 Survivor)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = setFromList [Tactic, Trick]
    , cdCriteria = Just (exists $ EnemyAt YourLocation <> oneOf [EnemyWithEvade, EnemyWithFight])
    , cdActions = [#parley]
    }

wrongPlaceRightTime :: CardDef
wrongPlaceRightTime =
  (event "10115" "Wrong Place, Right Time" 0 Survivor)
    { cdSkills = [#willpower, #agility, #wild]
    , cdCardTraits = setFromList [Spirit, Double]
    , cdAdditionalCost = Just (ActionCost 1)
    , cdCriteria =
        Just
          $ oneOf
            [ youExist InvestigatorWithAnyDamage <> exists (AssetAt YourLocation <> AssetWithHealth)
            , youExist InvestigatorWithAnyHorror <> exists (AssetAt YourLocation <> AssetWithSanity)
            ]
    }

keepFaith2 :: CardDef
keepFaith2 =
  (event "10124" "Keep Faith" 0 Survivor)
    { cdSkills = [#willpower, #willpower]
    , cdCardTraits = setFromList [Fortune, Blessed]
    , cdFastWindow = Just FastPlayerWindow
    , cdCriteria = Just Criteria.HasRemainingBlessTokens
    , cdLevel = Just 2
    }

bideYourTime :: CardDef
bideYourTime =
  (event "10129" "Bide Your Time" 0 Neutral)
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Double]
    , cdAdditionalCost = Just (ActionCost 1)
    }

dawnStar1 :: CardDef
dawnStar1 =
  (event "10131" "Dawn Star" 1 Neutral)
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Ritual, Blessed]
    , cdFastWindow = Just $ RevealChaosTokensDuringSkillTest #after Anyone SkillTestAtYourLocation #curse
    }
