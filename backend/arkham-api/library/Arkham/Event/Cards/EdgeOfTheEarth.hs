module Arkham.Event.Cards.EdgeOfTheEarth where

import Arkham.Asset.Uses qualified as Uses
import Arkham.Criteria qualified as Criteria
import Arkham.EncounterSet (EncounterSet (MemorialsOfTheLost))
import Arkham.Event.Cards.Import

toeToToe :: CardDef
toeToToe =
  (event "08020" "Toe to Toe" 0 Guardian)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Spirit, Tactic]
    , cdActions = [#fight]
    , cdBeforeEffect = True
    }

getBehindMe :: CardDef
getBehindMe =
  (event "08021" "\"Get behind me!\"" 0 Guardian)
    { cdSkills = [#willpower, #combat]
    , cdCardTraits = setFromList [Spirit, Tactic]
    , cdFastWindow = Just FastPlayerWindow
    }

gangUp1 :: CardDef
gangUp1 =
  (event "08022" "Gang Up" 3 Guardian)
    { cdSkills = [#willpower, #combat]
    , cdCardTraits = setFromList [Spirit, Synergy]
    , cdActions = [#fight]
    , cdLevel = Just 1
    }

sweepingKick1 :: CardDef
sweepingKick1 =
  (event "08023" "Sweeping Kick" 1 Guardian)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Spirit, Tactic, Trick]
    , cdActions = [#fight]
    , cdLevel = Just 1
    }

dodge2 :: CardDef
dodge2 =
  (event "08026" "Dodge" 0 Guardian)
    { cdSkills = [#willpower, #agility, #agility]
    , cdCardTraits = setFromList [Tactic]
    , cdFastWindow =
        Just
          $ EnemyAttacks
            #when
            (affectsOthers $ InvestigatorAt YourLocation)
            (CancelableEnemyAttack AnyEnemyAttack)
            AnyEnemy
    , cdLevel = Just 2
    }

onTheHunt3 :: CardDef
onTheHunt3 =
  (event "08028" "On the Hunt" 0 Guardian)
    { cdCardTraits = singleton Tactic
    , cdFastWindow = Just $ WouldDrawEncounterCard #when You #mythos
    , cdSkills = [#willpower, #intellect, #combat]
    , cdLevel = Just 3
    }

fangOfTyrthrha4 :: CardDef
fangOfTyrthrha4 =
  (event "08029" "Fang of Tyr'thrha" 3 Guardian)
    { cdCardTraits = singleton Spell
    , cdActions = [#fight]
    , cdSkills = [#combat, #combat, #agility, #agility]
    , cdLevel = Just 4
    , cdCriteria =
        Just
          $ exists
          $ CanFightEnemyWithOverride
          $ Criteria.CriteriaOverride
          $ Criteria.enemyExists
          $ EnemyAt RevealedLocation
    , cdOverrideActionPlayableIfCriteriaMet = True
    }

writtenInTheStars :: CardDef
writtenInTheStars =
  (event "08034" "Written in the Stars" 1 Seeker)
    { cdSkills = [#wild]
    , cdCardTraits = singleton Insight
    , cdFastWindow = Just $ DuringTurn You
    , cdCriteria = can.manipulate.deck
    }

joinTheCaravan1 :: CardDef
joinTheCaravan1 =
  (event "08036" "Join the Caravan" 5 Seeker)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = setFromList [Insight, Synergy]
    , cdFastWindow = Just $ DuringTurn You
    , cdCriteria = Just $ exists $ CanMoveToLocation You ThisCard RevealedLocation
    , cdLevel = Just 1
    , cdCardInHandEffects = True
    }

unearthTheAncients2 :: CardDef
unearthTheAncients2 =
  (event "08039" "Unearth the Ancients" 0 Seeker)
    { cdSkills = [#intellect, #intellect, #agility]
    , cdCardTraits = singleton Insight
    , cdActions = [#investigate]
    , cdCriteria = Just $ Criteria.ExtendedCardExists $ InHandOf You <> basic (#seeker <> #asset)
    , cdLevel = Just 2
    }

scoutAhead :: CardDef
scoutAhead =
  (event "08047" "Scout Ahead" 1 Rogue)
    { cdSkills = [#agility, #agility]
    , cdCardTraits = setFromList [Insight, Trick]
    , cdActions = [#move]
    , cdCriteria = Just $ youExist can.move
    }

twentyOneOrBust :: CardDef
twentyOneOrBust =
  (event "08048" "21 or Bust" 2 Rogue)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Fortune, Gambit]
    }

counterespionage1 :: CardDef
counterespionage1 =
  (event "08049" "Counterespionage" 2 Rogue)
    { cdSkills = [#willpower, #willpower]
    , cdCardTraits = setFromList [Favor, Service]
    , cdCriteria = Just $ oneOf [Criteria.EventWindowInvestigatorIs You, Criteria.CanAffordCostIncrease 2] -- WindowInvestigatorIs only handles draw card right now
    , cdFastWindow = Just $ DrawCard #when Anyone (basic NonWeaknessTreachery) AnyDeck
    , cdCardInHandEffects = True
    , cdLevel = Just 1
    }

cheatTheSystem1 :: CardDef
cheatTheSystem1 =
  (event "08050" "Cheat the System" 0 Rogue)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = setFromList [Trick, Synergy]
    , cdCriteria =
        Just
          $ youExist can.gain.resources
          <> Criteria.HasCalculation (DifferentClassAmong $ ControlledBy You) (atLeast 1)
    , cdFastWindow = Just FastPlayerWindow
    , cdLevel = Just 1
    }

untimelyTransaction1 :: CardDef
untimelyTransaction1 =
  (event "08051" "Untimely Transaction" 0 Rogue)
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Favor]
    , cdCriteria = Just $ youExist $ can.reveal.cards <> HandWith (HasCard $ CardWithTrait Item)
    , cdLevel = Just 1
    }

moneyTalks2 :: CardDef
moneyTalks2 =
  (event "08054" "Money Talks" 0 Rogue)
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Favor, Gambit]
    , cdFastWindow =
        Just
          $ InitiatedSkillTest
            #when
            (affectsOthers $ InvestigatorAt Anywhere)
            AnySkillType
            AnySkillTestValue
            #any
    , cdLevel = Just 2
    }

blackMarket2 :: CardDef
blackMarket2 =
  (event "08055" "Black Market" 1 Rogue)
    { cdCardTraits = setFromList [Favor]
    , cdCriteria =
        Just
          $ exists (affectsOthers $ can.manipulate.deck <> DeckWith (LengthIs $ atLeast 5))
          <> youExist can.reveal.cards
    , cdFastWindow = Just $ PhaseBegins #after #investigation
    , cdLevel = Just 2
    }

meditativeTrance :: CardDef
meditativeTrance =
  (event "08061" "Meditative Trance" 2 Mystic)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = setFromList [Insight, Spirit]
    , cdCriteria =
        Just
          $ youExist
          $ InvestigatorWithFilledSlot #arcane
          <> oneOf [HealableInvestigator ThisCard dType You | dType <- [#damage, #horror]]
    }

windsOfPower1 :: CardDef
windsOfPower1 =
  (event "08063" "Winds of Power" 2 Mystic)
    { cdSkills = [#willpower, #willpower]
    , cdCardTraits = setFromList [Spirit]
    , cdCriteria = Just $ exists (AssetControlledBy You <> AssetCanHaveUses Uses.Charge)
    , cdCardInHandEffects = True
    , cdLevel = Just 1
    }

foresight1 :: CardDef
foresight1 =
  (event "08064" "Foresight" 0 Mystic)
    { cdSkills = [#intellect, #intellect]
    , cdCardTraits = setFromList [Augury]
    , cdFastWindow =
        Just
          $ WouldDrawCard
            #when
            (affectsOthers $ InvestigatorAt YourLocation)
            (DeckOneOf [EncounterDeck, DeckOf ThatInvestigator])
    , cdLevel = Just 1
    }

parallelFates2 :: CardDef
parallelFates2 =
  (event "08066" "Parallel Fates" 0 Mystic)
    { cdSkills = [#willpower, #wild]
    , cdCardTraits = singleton Augury
    , cdLevel = Just 2
    , cdCriteria =
        Just $ exists $ oneOf [affectsOthers can.manipulate.deck, You <> can.target.encounterDeck]
    }

juryRig :: CardDef
juryRig =
  (event "08074" "Jury-Rig" 0 Survivor)
    { cdSkills = [#intellect, #combat]
    , cdCardTraits = singleton Upgrade
    , cdUses = Uses.Uses Uses.Durability (Fixed 3)
    , cdCriteria = Just $ exists $ #item <> AssetControlledBy (affectsOthers AtYourLocation)
    }

burnAfterReading1 :: CardDef
burnAfterReading1 =
  (event "08076" "Burn After Reading" 1 Survivor)
    { cdSkills = [#wild]
    , cdCardTraits = singleton Insight
    , cdCriteria =
        Just
          $ oneOf
            [ exists $ InHandOf You <> basic (CardWithMaxLevel 5)
            , canDiscoverCluesAtYourLocation
            ]
    , cdLevel = Just 1
    }

bloodWillHaveBlood2 :: CardDef
bloodWillHaveBlood2 =
  (event "08079" "Blood Will Have Blood" 1 Survivor)
    { cdSkills = [#willpower, #combat]
    , cdCardTraits = setFromList [Pact, Cursed]
    , cdFastWindow =
        Just
          $ EnemyAttacks #after You (not_ (AttackDamagedAsset #ally) <> AttackDealtDamageOrHorror) AnyEnemy
    , cdLimits = [MaxPerAttack 1]
    , cdCriteria = can.draw.cards
    , cdLevel = Just 2
    }

fendOff3 :: CardDef
fendOff3 =
  (event "08082" "Fend Off" 2 Survivor)
    { cdSkills = [#agility, #agility]
    , cdCardTraits = setFromList [Gambit, Trick]
    , cdFastWindow = Just $ EnemySpawns #when YourLocation NonEliteEnemy
    , cdLevel = Just 3
    }

onTheTrail1 :: CardDef
onTheTrail1 =
  (multiClassEvent "08084" "On the Trail" 1 [Guardian, Seeker])
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = setFromList [Insight, Tactic]
    , cdLevel = Just 1
    , cdCriteria =
        Just
          $ exists
          $ oneOf
            [ CanMoveCloserToLocation ThisCard You (LocationWithEnemy AnyEnemy)
            , LocationBetween
                YourLocation
                (LocationWithEnemy AnyEnemy)
                (EmptyLocation <> LocationWithDiscoverableCluesBy You)
            ]
    }

onTheTrail3 :: CardDef
onTheTrail3 =
  (multiClassEvent "08085" "On the Trail" 1 [Guardian, Seeker])
    { cdSkills = [#intellect, #intellect, #combat, #agility]
    , cdCardTraits = setFromList [Insight, Tactic]
    , cdLevel = Just 3
    , cdCriteria =
        Just $ exists (CanMoveCloserToLocation ThisCard You (LocationWithEnemy AnyEnemy))
    }

snipe1 :: CardDef
snipe1 =
  (multiClassEvent "08087" "Snipe" 0 [Guardian, Rogue])
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Tactic]
    , cdLevel = Just 1
    , cdAttackOfOpportunityModifiers = [DoesNotProvokeAttacksOfOpportunity]
    }

protectingTheAnirniq2 :: CardDef
protectingTheAnirniq2 =
  (multiClassEvent "08102" "Protecting the Anirniq" 1 [Seeker, Mystic])
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = setFromList [Ritual]
    , cdLevel = Just 2
    , cdFastWindow =
        let validOwner = oneOf [can.draw.cards, can.have.cards.leaveDiscard]
         in Just
              $ oneOf
                [ Discarded #after Nothing SourceIsCardEffect $ basic (#asset <> #ally) <> OwnedBy validOwner
                , AssetDefeated #after ByAny (#ally <> AssetOwnedBy validOwner)
                ]
    }

etherealSlip :: CardDef
etherealSlip =
  (multiClassEvent "08108" "Ethereal Slip" 2 [Rogue, Mystic])
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = setFromList [Spell, Trick]
    , cdCriteria =
        Just
          $ exists
          $ NonEliteEnemy
          <> EnemyAt (LocationWithDistanceFromAtMost 2 YourLocation (RevealedLocation <> CanEnterLocation You))
          <> EnemyCanEnter YourLocation
    }

etherealSlip2 :: CardDef
etherealSlip2 =
  (multiClassEvent "08110" "Ethereal Slip" 1 [Rogue, Mystic])
    { cdSkills = [#willpower, #agility, #agility]
    , cdCardTraits = setFromList [Spell, Trick]
    , cdCriteria =
        Just
          $ exists
          $ NonEliteEnemy
          <> EnemyAt (RevealedLocation <> CanEnterLocation You)
          <> EnemyCanEnter YourLocation
    , cdLevel = Just 2
    }

hitMe :: CardDef
hitMe =
  (multiClassEvent "08112" "\"Hit me!\"" 1 [Rogue, Survivor])
    { cdSkills = [#agility]
    , cdCardTraits = setFromList [Fortune, Gambit]
    , cdFastWindow = Just $ RevealChaosToken #after You #any
    , cdCriteria = Just Criteria.DuringAnySkillTest
    }

callForBackup2 :: CardDef
callForBackup2 =
  (event "08129" "Call for Backup" 1 Neutral)
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Favor, Synergy]
    , cdLevel = Just 2
    , cdCriteria =
        let control k = exists (ControlledBy You <> basic (CardWithClass k))
            healableCardExists d =
              oneOf
                [ exists (HealableAsset ThisCard d AnyAsset)
                , exists (HealableInvestigator ThisCard d Anyone)
                ]
         in Just
              $ oneOf
                [ control Rogue <> exists AccessibleLocation <> youExist can.move
                , control Guardian
                    <> Criteria.CanDealDamage
                    <> exists (EnemyAt YourLocation <> EnemyCanBeDamagedBySource ThisCard)
                , control Seeker <> canDiscoverCluesAtYourLocation
                , control Mystic <> healableCardExists #horror
                , control Survivor <> healableCardExists #damage
                ]
    }

dyersSketches :: CardDef
dyersSketches =
  (event "08733" "Dyer's Sketches" 2 Neutral)
    { cdSkills = [#intellect, #intellect]
    , cdCardTraits = singleton Insight
    , cdCriteria = Just $ can.draw.cards You
    , cdFastWindow = Just FastPlayerWindow
    , cdEncounterSet = Just MemorialsOfTheLost
    , cdEncounterSetQuantity = Just 1
    }

takadasCache :: CardDef
takadasCache =
  (event "08737" "Takada's Cache" 0 Neutral)
    { cdSkills = [#wild]
    , cdCardTraits = singleton Supply
    , cdFastWindow = Just FastPlayerWindow
    , cdEncounterSet = Just MemorialsOfTheLost
    , cdEncounterSetQuantity = Just 1
    }
