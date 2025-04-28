{-# LANGUAGE TemplateHaskell #-}

module Arkham.Modifier where

import Arkham.Action
import Arkham.Action.Additional
import Arkham.Asset.Uses
import {-# SOURCE #-} Arkham.Calculation
import {-# SOURCE #-} Arkham.Card (Card, CardCode)
import Arkham.Card.CardType
import {-# SOURCE #-} Arkham.Card.EncounterCard
import Arkham.ChaosBag.RevealStrategy
import Arkham.ChaosToken.Types
import Arkham.ClassSymbol
import {-# SOURCE #-} Arkham.Cost
import {-# SOURCE #-} Arkham.Criteria
import Arkham.Criteria.Override
import Arkham.Damage
import {-# SOURCE #-} Arkham.Enemy.Types
import Arkham.Field
import Arkham.Id
import Arkham.Json
import Arkham.Keyword
import Arkham.Matcher.Types
import Arkham.Phase
import Arkham.Prelude
import Arkham.Scenario.Deck
import Arkham.SkillType
import Arkham.SlotType
import Arkham.SortedPair
import {-# SOURCE #-} Arkham.Source
import {-# SOURCE #-} Arkham.Spawn
import {-# SOURCE #-} Arkham.Strategy
import {-# SOURCE #-} Arkham.Target
import Arkham.Trait
import Data.Aeson.TH
import GHC.OverloadedLabels

data ModifierType
  = CriteriaModifier Criterion ModifierType
  | OnCommitCardModifier InvestigatorId ExtendedCardMatcher ModifierType
  | AbilityModifier Target Int ModifierType
  | ActionCostModifier Int
  | ActionCostOf ActionTarget Int -- TODO: Don't use this for anything than decreasing
  | ActionCostSetToModifier Int
  | ActionDoesNotCauseAttacksOfOpportunity Action
  | ActionSkillModifier {action :: Action, skillType :: SkillType, value :: Int}
  | ActionsAreFree
  | AddChaosTokenValue ChaosTokenValue
  | AddKeyword Keyword
  | AddSkillIcons [SkillIcon]
  | AddSkillToOtherSkill SkillType SkillType
  | AddSkillValue SkillType
  | AddSkillValueOf SkillType InvestigatorId
  | AddTrait Trait
  | AdditionalActionCostOf ActionTarget Int
  | AdditionalActions Text Source Int
  | AdditionalCost Cost
  | AdditionalPlayCostOf ExtendedCardMatcher Cost
  | AdditionalCostToCommit InvestigatorId Cost
  | AdditionalCostToEnter Cost
  | AdditionalCostToEnterMatching LocationMatcher Cost
  | AdditionalCostToInvestigate Cost
  | AdditionalCostToLeave Cost
  | AdditionalCostToResign Cost
  | AdditionalResources Int
  | AdditionalSlot SlotType
  | AdditionalStartingCards [Card]
  | AdditionalStartingUses Int
  | AdditionalTargets Int
  | AlternateEvadeField (SomeField Enemy)
  | AlternateFightField (SomeField Enemy)
  | AlternateResourceCost CardMatcher Cost
  | AlternateSuccess Target
  | AlternateSuccessfullInvestigation Target
  | AlternateUpkeepDraw Target
  | AlternativeReady Source
  | AnySkillValue Int
  | AnySkillValueCalculated GameCalculation
  | AsIfAt LocationId
  | CanBeAttackedAsIfEnemy
  | AsIfEnemyFight Int
  | AsIfEngagedWith EnemyId
  | AsIfInHand Card
  | AsIfUnderControlOf InvestigatorId
  | AttackDealsEitherDamageOrHorror
  | AttacksCannotBeCancelled
  | Barricades [LocationId]
  | BaseSkill Int
  | BaseSkillOf {skillType :: SkillType, value :: Int}
  | BaseStartingResources Int
  | BecomesFast WindowMatcher
  | Blank
  | BlankExceptForcedAbilities
  | Blocked
  | BondedInsteadOfDiscard
  | BondedInsteadOfShuffle
  | BountiesOnly
  | CalculatedDifficulty GameCalculation
  | CanAssignDamageToAsset AssetId
  | CanAssignDamageToInvestigator InvestigatorId
  | CanAssignHorrorToAsset AssetId
  | CanAssignHorrorToInvestigator InvestigatorId
  | CanBeAssignedDirectDamage
  | CanBeFoughtAsIfAtYourLocation
  | CanBecomeFast CardMatcher
  | CanCommitToSkillTestPerformedByAnInvestigatorAt LocationMatcher
  | CanCommitToSkillTestsAsIfInHand Card
  | CanEnterEmptySpace
  | CanHealAtFull SourceMatcher DamageType
  | CanIgnoreAspect AspectMatcher
  | CanIgnoreBarriers
  | CanIgnoreLimit
  | CanModify ModifierType
  | CanMoveWith InvestigatorMatcher
  | CanOnlyBeAttackedByAbilityOn (Set CardCode)
  | CanOnlyBeDefeatedBy SourceMatcher
  | CanOnlyBeDefeatedByDamage
  | CanOnlyUseCardsInRole ClassSymbol
  | CanPlayFromDiscard CardMatcher
  | CanPlayTopOfDeck CardMatcher
  | CanPlayTopmostOfDiscard (Maybe CardType, [Trait])
  | CanPlayWithOverride CriteriaOverride
  | CanReduceCostOf CardMatcher Int
  | CanResolveToken ChaosTokenFace Target
  | CanRetaliateWhileExhausted
  | CanSpendResourcesOnCardFromInvestigator InvestigatorMatcher CardMatcher
  | CanSpendUsesAsResourceOnCardFromInvestigator AssetId UseType InvestigatorMatcher CardMatcher
  | CancelAnyChaosToken ChaosTokenMatcher
  | CancelAnyChaosTokenAndDrawAnother ChaosTokenMatcher
  | CancelAttacksByEnemies Card EnemyMatcher
  | CancelEffects
  | CancelSkills
  | Cancelled
  | CannotAffectOtherPlayersWithPlayerEffectsExceptDamage
  | CannotAssignDamage InvestigatorId
  | CannotAttack
  | CannotBeAdvancedByDoomThreshold
  | CannotBeAttacked
  | CannotBeAttackedBy EnemyMatcher
  | CannotBeAttackedByPlayerSourcesExcept SourceMatcher
  | CannotBeDamaged
  | CannotBeDamagedByPlayerSources SourceMatcher
  | CannotBeDamagedByPlayerSourcesExcept SourceMatcher
  | CannotBeDamagedBySourcesExcept SourceMatcher
  | CannotBeDefeated
  | CannotBeEngaged
  | CannotBeEngagedBy EnemyMatcher
  | CannotBeEnteredBy EnemyMatcher
  | CannotBeEvaded
  | CannotBeFlipped
  | CannotBeFlooded
  | CannotBeFullyFlooded
  | CannotBeHuntedBy EnemyMatcher
  | CannotBeMoved
  | CannotBeRevealed
  | CannotCancelHorror
  | CannotCancelHorrorFrom Source
  | CannotCancelOrIgnoreChaosToken ChaosTokenFace
  | CannotCommitCards CardMatcher
  | CannotCommitToOtherInvestigatorsSkillTests
  | CannotDealDamage
  | CannotDiscoverClues
  | CannotDiscoverCluesAt LocationMatcher
  | CannotMoveCluesFromHere
  | CannotDiscoverCluesExceptAsResultOfInvestigation LocationMatcher
  | CannotDisengageEnemies
  | CannotDrawCards
  | CannotDrawCardsFromPlayerCardEffects
  | CannotEngage InvestigatorId
  | CannotEnter LocationId
  | CannotEnterVehicle AssetMatcher
  | CannotEvade EnemyMatcher
  | CannotExplore
  | CannotFight EnemyMatcher
  | CannotGainResources
  | CannotGainResourcesFromPlayerCardEffects
  | CannotGainXP
  | CannotHaveAttachments
  | CannotHealDamage
  | CannotHealHorror
  | CannotHaveHorrorHealed
  | CannotHaveDamageHealed
  | CannotHealHorrorOnOtherCards Target
  | CannotInvestigate
  | CannotInvestigateLocation LocationId
  | CannotMakeAttacksOfOpportunity
  | CannotManipulateDeck
  | CannotMove
  | CannotMoveExceptByScenarioCardEffects
  | CannotMoveMoreThanOnceEachTurn
  | CannotMulligan
  | CannotParleyWith EnemyMatcher
  | CannotPerformSkillTest
  | CannotPlaceClues
  | CannotPlaceDoomOnThis
  | CannotPlay CardMatcher
  | CannotPutIntoPlay CardMatcher
  | CannotReady
  | CannotReplaceWeaknesses
  | CannotRevealCards
  | CannotSpawnIn LocationMatcher
  | CannotSpendClues
  | CannotSpendKeys
  | CannotTakeKeys
  | CannotTakeAction ActionTarget
  | CannotTakeControlOfClues
  | CannotTriggerAbilityMatching AbilityMatcher
  | CannotTriggerFastAbilities
  | CardsCannotLeaveYourDiscardPile
  | ChangeChaosTokenModifier ChaosTokenModifier
  | ChangeRevealStrategy RevealStrategy
  | DrawAdditionalChaosTokens Int
  | ChangeSpawnLocation LocationMatcher LocationMatcher
  | ChaosTokenFaceModifier [ChaosTokenFace]
  | ChaosTokenValueModifier Int
  | CheckHandSizeAfterDraw
  | ChuckFergus2Modifier CardMatcher Int -- Used by Chuck Fergus (2), check for notes
  | CommitCost Cost
  | ConnectedToWhen LocationMatcher LocationMatcher
  | ControlledAssetsCannotReady
  | CountAllDoomInPlay
  | CountsAsInvestigatorForHunterEnemies
  | DamageDealt Int
  | DamageDealtToInvestigator Int
  | DamageTaken Int
  | HorrorTaken Int
  | DamageTakenFrom DamageEffectMatcher Int
  | Difficulty Int
  | DiscoveredClues Int
  | DiscoveredCluesAt LocationId Int
  | DoNotDisengageEvaded
  | DoNotDrawChaosTokensForSkillChecks
  | DoNotDrawConnection (SortedPair LocationId)
  | DoNotExhaust
  | DoNotExhaustEvaded
  | DoNotRemoveDoom
  | DoNotRevealAnotherChaosToken -- see: Ancient Covenant (2)
  | DoNotTakeUpSlot SlotType
  | DoesNotDamageOtherInvestigator
  | DoesNotReadyDuringUpkeep
  | DoomSubtracts
  | DoomThresholdModifier Int
  | DoubleBaseSkillValue
  | DoubleDifficulty
  | DoubleNegativeModifiersOnChaosTokens
  | DoubleModifiersOnChaosTokens
  | DoubleSkillIcons
  | DoubleSuccess
  | DuringEnemyPhaseMustMoveToward Target
  | EffectsCannotBeCanceled
  | EnemyEngageActionCriteria CriteriaOverride
  | EnemyEvade Int
  | EnemyEvadeActionCriteria CriteriaOverride
  | EnemyEvadeWithMin Int (Min Int)
  | EnemyFight Int
  | EnemyFightActionCriteria CriteriaOverride
  | EnemyFightWithMin Int (Min Int)
  | EntersPlayWithDoom Int
  | ExhaustIfDefeated
  | ExtraResources Int
  | FailTies
  | FewerActions Int
  | FewerMatchingIconsPerCard Int
  | FewerSlots SlotType Int
  | ForEach GameCalculation [ModifierType]
  | ForcePrey PreyMatcher
  | ForceSpawn SpawnAt
  | ForceSpawnLocation LocationMatcher
  | ForcedChaosTokenChange ChaosTokenFace [ChaosTokenFace]
  | Foresight Text
  | GainVictory Int
  | GiveAdditionalAction AdditionalAction
  | HandSize Int
  | MaxHandSize Int
  | HandSizeCardCount Int
  | HealHorrorAsIfOnInvestigator Target Int
  | HealHorrorOnThisAsIfInvestigator InvestigatorId -- DEPRECATED
  | HealingTaken Int
  | HealthModifier Int
  | HealthModifierWithMin Int (Min Int)
  | HorrorDealt Int
  | HunterConnectedTo LocationId
  | IfFailureModifier ModifierType
  | IfSuccessfulModifier ModifierType
  | IgnoreActionCost
  | IgnoreAlert
  | IgnoreAllCosts
  | IgnoreAloof
  | IgnoreAttacksOfOpportunity
  | IgnoreBarriers
  | IgnoreChaosToken
  | IgnoreChaosTokenEffects
  | IgnoreChaosTokenModifier
  | IgnoreCommitOneRestriction
  | IgnoreDoomOnThis Int
  | IgnoreEngagementRequirement
  | IgnoreHandSizeReduction
  | IgnoreLimit
  | IgnoreOnSameLocation
  | IgnorePlayableModifierContexts
  | IgnoreRetaliate
  | IgnoreRevelation
  | IgnoreText
  | IgnoreTextOnLocation LocationMatcher
  | InVictoryDisplayForCountingVengeance
  | IncreaseCostOf ExtendedCardMatcher Int
  | InvestigateActionCriteria CriteriaOverride
  | IsEmptySpace
  | IsPointOfDamage
  | IsPointOfHorror
  | IsSpirit InvestigatorId
  | KilledIfDefeated
  | LeaveCardWhereItIs
  | LookAtDepth Int
  | LosePatrol
  | LoseVictory
  | MaxCluesDiscovered Int
  | MaxDamageTaken Int
  | MayChooseNotToTakeUpkeepResources
  | MayChooseToRemoveChaosToken InvestigatorId
  | MayIgnoreAttacksOfOpportunity
  | MayIgnoreLocationEffectsAndKeywords
  | MetaModifier Value
  | ModifierIfSucceededBy Int Modifier
  | Mulligans Int
  | MustBeCommitted
  | MustChooseEnemy EnemyMatcher
  | MustFight EnemyId
  | MustTakeAction ActionTarget
  | NegativeToPositive
  | NoDamageDealt
  | NoInitialSwarm
  | NoMoreThanOneDamageOrHorrorAmongst AssetMatcher
  | NoStandardDamage
  | NoSurge
  | NonDirectDamageMustBeAssignToThisFirst
  | NonDirectDamageMustBeAssignToThisN Int
  | NonDirectHorrorMustBeAssignToThisFirst
  | NonTraitRestrictedModifier Trait ModifierType
  | Omnipotent
  | OnlyFirstCopyCardCountsTowardMaximumHandSize
  | OtherDoomSubtracts
  | PlaceOnBottomOfDeckInsteadOfDiscard
  | PlayUnderControlOf InvestigatorId
  | PlayableCardOf InvestigatorId Card
  | PlayableModifierContexts [(CardMatcher, [ModifierType])]
  | ProvidesProxyUses UseType UseType Source
  | ProvidesUses UseType Source
  | ReduceCostOf CardMatcher Int
  | ReduceStartingCluesByHalf
  | RemoveFromGameInsteadOfDiscard
  | RemoveKeyword Keyword
  | RemoveSkillIcons [SkillIcon]
  | RemoveTrait Trait
  | ReplaceAllSkillIconsWithWild
  | ResolveEffectsAgain -- NOTE: If used for more than Tekelili, need to figure out what to do
  | ResolvesFailedEffects
  | ReturnBlessedToChaosBag
  | ReturnCursedToChaosBag
  | ReturnToHandAfterTest
  | RevealAnotherChaosToken -- TODO: Only ShatteredAeons handles this, if a player card affects this, all scenarios have to be updated, we also use this for Cats of Ulthar directly on the SkillTest
  | RevealChaosTokensBeforeCommittingCards
  | SanityModifier Int
  | ScenarioModifier Text
  | ScenarioModifierValue Text Value
  | SearchDepth Int
  | Semaphore
  | SetAbilityCost Cost
  | SetAbilityCriteria CriteriaOverride
  | SetAfterPlay AfterPlayStrategy
  | SetAttackDamageStrategy DamageStrategy
  | SetDifficulty Int
  | SetShroud Int
  | SetSkillValue { skillType :: SkillType, value :: Int }
  | SharesSlotWith Int CardMatcher -- card matcher allows us to check more easily from hand
  | ShroudModifier Int
  | ShuffleIntoAnyDeckInsteadOfDiscard
  | ShuffleIntoDeckInsteadOfDiscard
  | SkillCannotBeIncreased SkillType
  | CalculatedSkillModifier {skillType :: SkillType, calculation :: GameCalculation}
  | SkillModifier {skillType :: SkillType, value :: Int}
  | SkillModifiersAffectOtherSkill SkillType SkillType
  | SkillTestAutomaticallySucceeds
  | SkillTestResultValueModifier Int
  | SkipMythosPhaseStep MythosPhaseStep
  | SlotCanBe SlotType SlotType
  | SpawnNonEliteAtConnectingInstead
  | StartingClues Int
  | StartingHand Int
  | StartingResources Int
  | StartsInEncounterDeck EncounterCard
  | SwarmingValue Int
  | TakeUpFewerSlots SlotType Int
  | TopCardOfDeckIsRevealed
  | TopCardOfEncounterDeckIsRevealed
  | TraitRestrictedModifier Trait ModifierType
  | TreatAllDamageAsDirect
  | CancelOneDamageOrHorror
  | TreatRevealedChaosTokenAs ChaosTokenFace
  | UpgradeTargetIfAble Target
  | UpkeepResources Int
  | UseEncounterDeck ScenarioEncounterDeckKey -- The Wages of Sin
  | UseSkillInPlaceOf SkillType SkillType -- oh no, why are these similar, this let's you choose
  | UseSkillInsteadOf SkillType SkillType -- this doesn't
  | VehicleCannotMove
  | WillCancelHorror Int
  | XPModifier Text Int
  | UIModifier UIModifier
  | BecomeHomunculusWhenDefeated
  | BecomeInvestigator InvestigatorId
  | DrawsEachEncounterCard
  deriving stock (Show, Eq, Ord, Data)

data UIModifier
  = Ethereal -- from Ethereal Form
  | Explosion -- from Dyanamite Blast
  | Locus -- from Prophesiae Profana
  deriving stock (Show, Eq, Ord, Data)

instance IsLabel "combat" (Int -> ModifierType) where
  fromLabel = SkillModifier #combat

instance IsLabel "agility" (Int -> ModifierType) where
  fromLabel = SkillModifier #agility

instance IsLabel "agility" (Integer -> ModifierType) where
  fromLabel = SkillModifier #agility . fromIntegral

instance IsLabel "intellect" (Int -> ModifierType) where
  fromLabel = SkillModifier #intellect

instance IsLabel "willpower" (Int -> ModifierType) where
  fromLabel = SkillModifier #willpower

instance IsLabel "damage" (Int -> ModifierType) where
  fromLabel = DamageDealt

data Modifier = Modifier
  { modifierSource :: Source
  , modifierType :: ModifierType
  , modifierActiveDuringSetup :: Bool
  , modifierCard :: Maybe Card
  }
  deriving stock (Show, Eq, Ord, Data)

overModifierTypeM :: Monad m => (ModifierType -> m ModifierType) -> Modifier -> m Modifier
overModifierTypeM f m = f (modifierType m) <&> \mt -> m {modifierType = mt}

setActiveDuringSetup :: Modifier -> Modifier
setActiveDuringSetup m = m {modifierActiveDuringSetup = True}

mconcat
  [ deriveToJSON defaultOptions ''ModifierType
  , [d|
      instance FromJSON ModifierType where
        parseJSON = withObject "ModifierType" \v -> do
          tag :: Text <- v .: "tag"
          case tag of
            "SetSkillValue" -> do
              contents <- (Left <$> v .: "contents") <|> (Right <$> (SetSkillValue <$> v .: "skillType" <*> v .: "value"))
              case contents of
                Left (a, b) -> pure $ SetSkillValue a b
                Right a -> pure a
            "CanPlayFromDiscard" -> do
              contents <- (Left <$> v .: "contents") <|> (Right <$> v .: "contents")
              case contents of
                Left (mType, traits :: [Trait]) -> pure $ CanPlayFromDiscard $ maybe AnyCard CardWithType mType <> foldMap CardWithTrait traits
                Right matcher -> pure $ CanPlayFromDiscard matcher
            "IncreaseCostOf" -> do
              contents <- (Left <$> v .: "contents") <|> (Right <$> v .: "contents")
              case contents of
                Right (matcher, n) -> pure $ IncreaseCostOf matcher n
                Left (matcher, n) -> pure $ IncreaseCostOf (BasicCardMatch matcher) n
            "Explosion" -> pure $ UIModifier Explosion
            "Locus" -> pure $ UIModifier Locus
            "Ethereal" -> pure $ UIModifier Ethereal
            "CanHealAtFull" -> do
              contents <- (Left <$> v .: "contents") <|> (Right <$> v .: "contents")
              case contents of
                Left n -> pure $ CanHealAtFull AnySource n
                Right (s, n) -> pure $ CanHealAtFull s n
            "CanBecomeFastOrReduceCostOf" -> do
              contents <- v .: "contents"
              pure $ uncurry ChuckFergus2Modifier contents
            "XPModifier" -> do
              contents <- (Left <$> v .: "contents") <|> (Right <$> v .: "contents")
              case contents of
                Left n -> pure $ XPModifier "Card Effect" n
                Right (s, n) -> pure $ XPModifier s n
            _ -> $(mkParseJSON defaultOptions ''ModifierType) (Object v)
      |]
  , deriveJSON (aesonOptions $ Just "modifier") ''Modifier
  , deriveJSON defaultOptions ''UIModifier
  , makePrisms ''ModifierType
  ]
