{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -O0 -fomit-interface-pragmas -fno-specialise #-}

module Arkham.Modifier where

import {-# SOURCE #-} Arkham.Ability.Types
import Arkham.Action
import Arkham.Action.Additional
import Arkham.Asset.Uses
import {-# SOURCE #-} Arkham.Calculation
import {-# SOURCE #-} Arkham.Card (Card, CardCode)
import Arkham.Card.CardType
import {-# SOURCE #-} Arkham.Card.EncounterCard
import Arkham.Card.Id
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
import Arkham.LocationSymbol (LocationSymbol)
import Arkham.Matcher.Types
import Arkham.Phase
import {-# SOURCE #-} Arkham.Placement
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
import GHC.Records

data ModifierType
  = CriteriaModifier Criterion ModifierType
  | OnCommitCardModifier InvestigatorId ExtendedCardMatcher ModifierType
  | AbilityModifier Target Int ModifierType
  | ActIsAgenda
  | ActionCostModifier Int
  | ActionCostOf ActionTarget Int -- TODO: Don't use this for anything than decreasing
  | ActionCostSetToModifier Int
  | ActionDoesNotCauseAttacksOfOpportunity Action
  | {- | An action taken while this is on the investigator still counts as an
    action they performed, but is invisible to the "same type of action" checks:
    it opens no 'PerformedSameTypeOfAction' window of its own, and the next
    action compares itself against the action before it instead. For "that
    action ignores this card's forced effect", where the granted action would
    otherwise become the thing the real action is measured against.
    -}
    ActionDoesNotCountAsRepeatedAction
  | ActionSkillModifier {action :: Action, skillType :: SkillType, value :: Int}
  | ActionsAreFree
  | AddChaosTokenValue ChaosTokenValue
  | AddKeyword Keyword
  | AddSkillIcons [SkillIcon]
  | AddSkillToOtherSkill SkillType SkillType
  | AddSkillValue SkillType
  | AddSkillValueOf SkillType InvestigatorId
  | WhileEnemyMovingModifier ModifierType
  | AddTrait Trait
  | AdditionalActionCostOf ActionTarget Int
  | AdditionalActions Text Source Int
  | AdditionalCost Cost
  | NoAdditionalCosts
  | AdditionalPlayCostOf ExtendedCardMatcher Cost
  | AdditionalCostToCommit InvestigatorId Cost
  | {- | An extra cost to take a particular action while at the location that
    carries this modifier (Dark Matter's Cold Wastes taxes the Scan action).
    Gathered from the acting investigator's location, like
    'AdditionalCostToResign'.
    -}
    AdditionalCostToPerformAction ActionTarget Cost
  | AdditionalCostToEnter Cost
  | AdditionalCostToEnterMatching LocationMatcher Cost
  | AdditionalCostToExplore Cost
  | AdditionalCostToInvestigate Cost
  | AdditionalCostToLeave Cost
  | {- | An extra cost to move, carried by the moving investigator rather than by a
    location. It is a cost of the move itself, so it is paid before the would-move
    and leaving windows instead of on entering (Lost in the Dark).
    -}
    AdditionalCostToMove Cost
  | AdditionalCostToResign Cost
  | AdditionalResources Int
  | {- | "Resolve its revelation effect an additional time." Adds N extra copies
    of the revelation itself; the surrounding @When@/@After (Revelation ...)@
    pair still runs exactly once, so the card is still discarded once, marked
    resolved once, and surges at most once.
    -}
    AdditionalRevelations Int
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
  | {- | "as if you were at that location in addition to your location". Unlike
    'AsIfAt' this does not replace 'InvestigatorLocation'; both the physical
    location and this one satisfy the @Here@ criterion.
    -}
    AsIfAlsoAt LocationId
  | CanBeAttackedAsIfEnemy
  | CanPlayUnderControlOf CardMatcher InvestigatorMatcher
  | AsIfEnemyFight Int
  | AsIfEngagedWith EnemyId
  | AsIfNotEngagedWith EnemyId
  | AsIfInHand Card
  | AsIfInHandFor ForPlay CardId
  | -- out-of-play effects only, never treated as in hand
    AsIfInHandForEffects CardId
  | AsIfResourcePool AssetId
  | AsIfUnderControlOf InvestigatorId
  | AsIfTurn InvestigatorId
  | EnemyAttacksOverride InvestigatorMatcher
  | AttackDealsEitherDamageOrHorror
  | {- | On an INVESTIGATOR: the standard range of their attacks reaches this many
    locations further. Read by cards worded relative to that range -- Springfield
    M1903's taboo adds a location to it.
    -}
    AttackRangeIncrease Int
  | AttacksCannotBeCancelled
  | Barricades [LocationId]
  | BaseShroud Int
  | BaseSkill Int
  | BaseSkillOf {skillType :: SkillType, value :: Int}
  | BaseSkillOfCalculated {skillType :: SkillType, calculation :: GameCalculation}
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
  | CanPlayAtLocation CardMatcher LocationMatcher
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
  | CancelEachCommittedCard
  | Cancelled
  | CannotAffectOtherPlayersWithPlayerEffectsExceptDamage
  | CannotAssignDamage InvestigatorId
  | CannotAttack
  | CannotAttackDuringEnemyPhase
  | CannotAttackEnemy EnemyId
  | CannotBeAdvancedByDoomThreshold
  | CannotBeAttacked
  | CannotBeAttackedBy EnemyMatcher
  | CannotBeAttackedByPlayerSourcesExcept SourceMatcher
  | CannotBeEvadedByPlayerSourcesExcept SourceMatcher
  | CannotReceiveModifiersFromPlayerSources
  | CannotBeExhaustedBy SourceMatcher
  | CannotBeDamaged
  | CannotBeDamagedByPlayerSources SourceMatcher
  | CannotBeDamagedByPlayerSourcesExcept SourceMatcher
  | CannotBeDamagedBySourcesExcept SourceMatcher
  | CannotBeDefeated
  | CannotBeDefeatedBy SourceMatcher
  | CannotBeEngaged
  | CannotBeEngagedBy EnemyMatcher
  | CannotBeEngagedByPlayerSourcesExcept SourceMatcher
  | CannotBeEnteredBy EnemyMatcher
  | CannotBeEvaded
  | CannotBeFlipped
  | CannotBeFlooded
  | CannotBeFullyFlooded
  | CannotBeHuntedBy EnemyMatcher
  | CannotBeMoved
  | CannotBeMovedBy SourceMatcher
  | CannotBeDisengagedBy SourceMatcher
  | CannotBeRemovedBy SourceMatcher
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
  | {- | This investigator is left out of the game's own "each investigator draws
    an encounter card" entirely -- out of the draw and out of Gloria Goldberg's
    draw-order choice, rather than handed a draw that then does nothing. Honored
    by 'Arkham.Message.AllDrawEncounterCard' and its per-investigator
    @ForInvestigator@, which is the mythos draw and the handful of cards that
    re-run it.

    It does NOT cover an effect that names this investigator and tells them to
    draw an encounter card; 'CannotDrawCards' does not either (it gates what may
    be offered and the opening hand, not a draw the game pushes). An out-of-play
    seat that must be unreachable by everything needs its own enumerated list of
    bans -- see /Ritual Circle: A Harnessed Future/.
    -}
    CannotDrawEncounterCards
  | {- | "Cannot willingly end their turn". Drops the end-turn button from the
    player window, and nothing else: an effect that ends the turn for you is
    not the investigator choosing to, so it still resolves.

    Only while the window offers something else. A window with no other option
    keeps the button, since there would otherwise be no way out of the turn at
    all; see 'Arkham.Investigator.Runner.Action.handlePlayerWindow'. Whatever
    condition the card attaches to -- /Ritual Circle: A Harnessed Future/ holds
    you while you have actions remaining -- belongs in the matcher that applies
    this, not here.
    -}
    CannotEndTurn
  | CannotEngage InvestigatorId
  | CannotEnter LocationId
  | {- | "You cannot enter X except by <source>". Unlike 'CannotEnter' this is
    source-aware, so it is only honored by 'getCanMoveToLocations_', the one
    move query that knows which effect is doing the moving.
    -}
    CannotEnterExcept LocationId SourceMatcher
  | CannotEnterVehicle AssetMatcher
  | CannotEvade EnemyMatcher
  | CannotExplore
  | CannotFight EnemyMatcher
  | CannotGainAdditionalActions
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
  | CancelMovement MovementId
  | CannotMoveExceptByScenarioCardEffects
  | CannotMulligan
  | CannotParleyWith EnemyMatcher
  | CannotPerformSkillTest
  | CannotPlaceClues
  | CannotPlaceDoomOnThis
  | CannotRemoveDoomOnThis
  | CannotPlay CardMatcher
  | CannotPutIntoPlay CardMatcher
  | CannotReady
  | CannotReplaceWeaknesses
  | CannotRevealCards
  | CannotSealChaosToken ChaosTokenFace
  | CannotSpawnIn LocationMatcher
  | CannotSpendClues
  | CannotSpendKeys
  | CannotTakeKeys
  | CannotLeavePlay
  | CannotLeaveYourHand
  | CannotTakeAction ActionTarget
  | CannotPerformAction ActionTarget
  | CannotTakeControlOfClues
  | CannotTriggerAbilityMatching AbilityMatcher
  | CannotTriggerFastAbilities
  | {- | An asset played with no room for it stays anyway, unslotted, rather
    than discarding something to make room or being discarded itself. Natalie
    Soulier, who then sweeps them up again at the start of upkeep.
    -}
    KeepAssetsInExcessOfSlots
  | CardsCannotLeaveYourDiscardPile
  | ChangeChaosTokenModifier ChaosTokenModifier
  | ChangeRevealStrategy RevealStrategy
  | DrawAdditionalChaosTokens Int AdditionalReveals
  | ChangeSpawnLocation LocationMatcher LocationMatcher
  | ChangeSpawnWith InvestigatorId SpawnAt
  | ChaosTokenFaceModifier [ChaosTokenFace]
  | ChaosTokenValueModifier Int
  | CheckHandSizeAfterDraw
  | ChuckFergus2Modifier CardMatcher Int -- Used by Chuck Fergus (2), check for notes
  | CommitCost Cost
  | ConnectedToWhen LocationMatcher LocationMatcher
  | ForMovementConnectedToWhen LocationMatcher LocationMatcher
  | {- | On the MOVER (investigator or enemy), not on a location: "while moving, treat
    your location as if it were connected to ...". Connection queries read the start
    location's modifiers, so a mover-scoped connection cannot be expressed as a
    location modifier; the movement helpers inject this onto the mover's location for
    the duration of the query, the way hunter movement already does for
    'HunterConnectedTo'.
    -}
    MovesAsIfConnectedTo LocationMatcher
  | ControlledAssetsCannotReady
  | {- | "Your exhausted cards cannot ready", which is wider than
    'ControlledAssetsCannotReady': an event in play can be exhausted too, and a
    card that says /cards/ rather than /assets/ has to reach it.
    -}
    ControlledCardsCannotReady
  | CountAllDoomInPlay
  | CountsAsInvestigatorForHunterEnemies
  | CountsAsDifferentLocation
  | DamageDealt Int
  | DamageDealtCalculation GameCalculation
  | DamageDealtToInvestigator Int
  | DamageTaken Int
  | HorrorTaken Int
  | DamageTakenFrom DamageEffectMatcher Int
  | Difficulty Int
  | DiscoveredClues Int
  | DiscoveredCluesAt LocationId Int
  | DoNotCollectResourcesDuringUpkeep
  | DoNotDisengageEvaded
  | DoNotDrawChaosTokensForSkillChecks
  | DoNotDrawConnection (SortedPair LocationId)
  | DoNotExhaust
  | DoNotExhaustEvaded
  | DoNotRemoveDoom
  | DoNotRevealAnotherChaosToken -- see: Ancient Covenant (2)
  | DoNotTakeUpSlot SlotType
  | DoNotTakeUpSlots
  | {- | Isolates a location's connections: it connects to nothing and nothing
    connects to it, while staying in play for everything else. Wired outbound in
    'Arkham.Helpers.Location.connectedMatcherParts' and inbound in the
    @LocationWithSymbol@ branch of 'Arkham.Game.getLocationsMatching'. Unlike
    'LosesConnectionSymbol', which drops one symbol and is outbound-only.
    -}
    DoesNotConnect
  | DoesNotDamageOtherInvestigator
  | DoesNotReadyDuringUpkeep
  | DoomSubtracts
  | DoomThresholdModifier Int
  | DoubleBaseSkillValue
  | DoubleDifficulty
  | DoubleNegativeModifiersOnChaosTokens
  | DoubleModifiersOnChaosTokens
  | {- | Notify the card this many extra times when a chaos token it is waiting
    on is revealed, so its "when/if/after you reveal" effect resolves again.
    -}
    ResolveEffectsAdditionalTimes Int
  | DoubleSkillIcons
  | {- | Double only the listed icons on a committed card, leaving the rest
    (notably @WildIcon@) counted once.
    -}
    DoubleSkillIconsOf [SkillIcon]
  | DoubleSuccess
  | DuringEnemyPhaseMustMoveToward Target
  | EffectsCannotBeCanceled
  | CannotCancelCardOrGameEffects
  | CannotIgnoreCardOrGameEffects
  | DrawGainsPeril
  | EnemyEngageActionCriteria CriteriaOverride
  | EnemyEvade Int
  | SwapFightAndEvade
  | EnemyEvadeActionCriteria CriteriaOverride
  | EnemyEvadeWithMin Int (Min Int)
  | EnemyFight Int
  | EnemyFightActionCriteria CriteriaOverride
  | EnemyFightWithMin Int (Min Int)
  | EntersPlayWithDoom Int
  | CanEvadeOverride CriteriaOverride
  | ExhaustIfDefeated
  | ExtraResources Int
  | FailTies
  | FewerActions Int
  | FewerMatchingIconsPerCard Int
  | {- | Ignore this many matching icons committed to each skill test this
    investigator performs, across every card committed to it, rather than
    'FewerMatchingIconsPerCard''s per-card reckoning.
    -}
    FewerMatchingIcons Int
  | FewerSlots SlotType Int
  | ForEach GameCalculation [ModifierType]
  | ForceConcealedPlacement Placement
  | ForcePrey PreyMatcher
  | ForceSpawn SpawnAt
  | ForceSpawnLocation LocationMatcher
  | ForcedChaosTokenChange ChaosTokenFace [ChaosTokenFace]
  | Foresight Text
  | {- | On a 'ChaosTokenFaceTarget': this face also carries every chaos token effect
    DECLARED for the named face, so the skill test window predicts it.

    A declaration only (see 'abilityChaosTokenEffects') -- the card that broadens the
    faces still resolves the effect itself. The Masque of the Red Death's agendas print
    exactly this: "add each [skull] effect on your location to each [cultist] and
    [tablet] token you reveal during skill tests."
    -}
    GainsChaosTokenEffectsOf ChaosTokenFace
  | GainVictory Int
  | GiveAdditionalAction AdditionalAction
  | HandSize Int
  | Hollow CardId
  | MaxHandSize Int
  | HandSizeCardCount Int
  | HealHorrorAsIfOnInvestigator Target Int
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
  | IgnoreChaosToken
  | IgnoreChaosTokenEffects
  | IgnoreChaosTokenModifier
  | {- | The symbol's revealed effects do not resolve, but the token's numeric
    modifier is untouched, so a replacement value (see The Black Cat (5))
    still applies. 'IgnoreChaosTokenEffects' would zero the value as well.
    -}
    IgnoreChaosTokenSymbolEffects
  | IgnoreCommitOneRestriction
  | {- | The tokens stay on the card and are still clues to the card itself, but
    @n@ of them are subtracted from every clue count the engine reads off it:
    the controller's 'InvestigatorClues' (so they are not spendable), the clue
    matchers (so the card is not offered as a clue source), and anything
    aggregating 'AssetClues'. The mirror of 'IgnoreDoomOnThis'.
    -}
    IgnoreCluesOnThis Int
  | IgnoreDoomOnThis Int
  | IgnoreEngagementRequirement
  | IgnoreHandSizeReduction
  | IgnoreLimit
  | IgnoreOnSameLocation
  | IgnorePlayableModifierContexts
  | IgnoreRetaliate
  | IgnoreRevelation
  | {- | On an INVESTIGATOR: drop the always-on skill bonuses their assets publish
    onto them. Read at collection time in 'Arkham.Helpers.Modifiers.getModifiers'',
    beside 'CannotReceiveModifiersFromPlayerSources', which is far broader --- that one
    takes every modifier from every player card (health, sanity, slots, actions), while
    this one takes only skill bonuses, and only from assets.

    "A bonus to a skill" is read narrowly and only at the top level: a positive
    'SkillModifier', 'ActionSkillModifier' or 'AnySkillValue'. A NEGATIVE value is a
    penalty, not a bonus, and is left alone --- the wording this exists for is "lose all
    bonuses", which never improves a skill. The calculated forms
    ('CalculatedSkillModifier', 'AnySkillValueCalculated') are also left alone, since
    their sign is not knowable without evaluating a 'GameCalculation', which this filter
    cannot do without re-entering modifier collection. Skill /substitution/
    ('AddSkillValue', 'AddSkillToOtherSkill', 'UseSkillInPlaceOf'), base-value
    ('BaseSkill', 'SetSkillValue', 'DoubleBaseSkillValue') and commit-icon
    ('AddSkillIcons') modifiers are not bonuses and are untouched.

    A bonus counts as "printed on the asset" only when the asset entity publishes it
    itself --- @controllerGets@ and friends, source exactly @AssetSource@ (an
    @AbilitySource (AssetSource _) _@ does NOT match) and no 'modifierEffect' stamp. A
    bonus a card ADDS DURING AN ACTION arrives through a window-modifier effect
    instead ('skillTestModifier', 'modifySkill'), which 'Arkham.Effect.Types' stamps
    with its effect id, so those survive. That split is what lets the clause carry its
    usual parenthetical: Holy Rosary's +1 [willpower] and Boxing Gloves' +1 [combat]
    while fighting go away, while .45 Automatic's and Machete's +1 [combat] from
    activating them do not.
    -}
    IgnoreSkillBonusesFromAssets
  | RevelationModifier Source ModifierType
  | IgnoreText
  | IgnoreTextOnLocation LocationMatcher
  | InVictoryDisplayForCountingVengeance
  | IncreaseCostOf ExtendedCardMatcher Int
  | {- | A composite enemy: several enemy cards that are a single enemy on the map
    (Cthulhu and the facets on his Cthulhu Board). The card carrying this is never
    itself fought or evaded; interacting with it means choosing one of the members.
    Written with 'Arkham.Helpers.Modifiers.interactAsOneOf', which pairs it with the
    @Cannot*@ modifiers that keep the card itself off every target list.
    -}
    InteractAsOneOf EnemyMatcher
  | InvestigateActionCriteria CriteriaOverride
  | IsEmptySpace
  | IsPointOfDamage
  | IsPointOfHorror
  | IsSpirit InvestigatorId
  | KilledIfDefeated
  | LeaveCardWhereItIs
  | LookAtDepth Int
  | LosePatrol
  | LosesConnectionSymbol LocationSymbol
  | ForcePatrol LocationMatcher
  | LoseVictory
  | MaxCluesDiscovered Int
  | MaxDamageTaken DamageEffectMatcher Int
  | MayChooseNotToTakeUpkeepResources
  | MayChooseToRemoveChaosToken InvestigatorId
  | MayIgnoreAttacksOfOpportunity
  | MayIgnoreAttacksOfOpportunityOf EnemyMatcher
  | MayIgnoreLocationEffectsAndKeywords
  | MetaModifier Value
  | ModifierIfSucceededBy Int Modifier
  | MovingToDoesNotProvokeAttacksOfOpportunity LocationMatcher
  | Mulligans Int
  | MustBeCommitted
  | MustChooseEnemy EnemyMatcher
  | MustFight EnemyId
  | MustTakeAction ActionTarget
  | MustPerformAbilityIfCan AbilityRef
  | {- | This investigator has a turn that was suspended mid-turn and is owed the
    rest of it. Carried from the moment of suspension until the resumed turn
    ends, and honored in three places, all in the investigation phase:

    * 'Arkham.Message.ChooseTurnPlayer' narrows the offer to the investigators
      carrying this while any of them still has a turn coming, so a resumed turn
      comes before anyone else's. Several of them are still /chosen between/
      rather than ordered for the players.
    * that same choice resolves as 'Arkham.Question.ResumeTurnPlayer' rather
      than @SetTurnPlayer@, so the turn is picked back up instead of begun: no
      @TurnBegins@ windows, and the investigator lands in 2.2.1.
    * @Begin InvestigationPhase@ leaves out its own player window between 2.1
      and 2.2, because a turn that resumes as the phase begins is already in
      progress -- the phase never reaches a point between turns.

    /Ritual Circle: A Harnessed Future/ is what wants it. What it still cannot
    carry across the phase boundary is per-turn bookkeeping: @actionsTaken@,
    @actionsPerformed@ and per-turn ability limits are reset by @Do BeginRound@
    before the phase is reached.
    -}
    ResumesSuspendedTurn
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
  | Persist
  | OnlyFirstCopyCardCountsTowardMaximumHandSize
  | OtherDoomSubtracts
  | {- | Like 'ForceSpawn', but only replaces an enemy's normal spawn location
    (a scenario rule, e.g. Dead Heat forcing Ghoul/Risen enemies to a random
    location). A 'ForceSpawn' from a drawing effect (On the Hunt, Kicking the
    Hornet's Nest) takes precedence over this.
    -}
    OverwrittenSpawn SpawnAt
  | PlaceOnBottomOfDeckInsteadOfDiscard
  | -- | Player cards that would be discarded are placed beneath the target instead
    PlaceUnderneathInsteadOfDiscard Target
  | PlaySource Source
  | PlayUnderControlOf InvestigatorId
  | PlayableCardOf InvestigatorId Card
  | PlayableModifierContexts [(CardMatcher, [ModifierType])]
  | ProvidesProxyUses UseType UseType Source
  | ProvidesUses UseType Source
  | ReduceCostOf CardMatcher Int
  | ReduceStartingCluesByHalf
  | RemoveFromGameInsteadOfDiscard
  | RemoveKeyword Keyword
  | RemoveConcealed
  | RemoveSkillIcons [SkillIcon]
  | RemoveTrait Trait
  | ReplaceAllSkillIconsWithWild
  | ResolveHunterTwice
  | SkillIconsSubtract
  | ResolveEffectsAgain -- NOTE: If used for more than Tekelili, need to figure out what to do
  | ResolveEffectsAgainMatch CardMatcher -- NOTE: If used for more than Tekelili, need to figure out what to do
  | ResolvesFailedEffects
  | ReturnBlessedToChaosBag
  | ReturnCursedToChaosBag
  | ReturnToHandAfterTest
  | RevealAnotherChaosToken -- TODO: Only ShatteredAeons handles this, if a player card affects this, all scenarios have to be updated, we also use this for Cats of Ulthar directly on the SkillTest
  | RevealChaosTokensBeforeCommittingCards
  | SanityModifier Int
  | CampaignModifier Text
  | InvestigatorModifier Text
  | ScenarioModifier Text
  | ScenarioModifierValue Text Value
  | SearchDepth Int
  | {- | When searching, also fold in every other investigator's matching
    deck/hand/discard cards, keyed under the same plain Zone. See Leah Atwood
    Codex 2 (Fate of the Vale).
    -}
    SearchAllInvestigators
  | {- | When searching, also fold this investigator's deck into the FromDeck
    zone (their cards keep pcOwner). Used to extend a search into a single
    chosen deck. See Leah Atwood Codex 2 (Fate of the Vale).
    -}
    SearchIncludesDeckOf InvestigatorId
  | Semaphore
  | SetAbilityCost Cost
  | SetAbilityCriteria CriteriaOverride
  | SetAfterPlay AfterPlayStrategy
  | SetAttackDamageStrategy DamageStrategy
  | SetDifficulty Int
  | SetShroud Int
  | SetSkillValue {skillType :: SkillType, value :: Int}
  | SharesSlotWith Int CardMatcher -- card matcher allows us to check more easily from hand
  | ShroudModifier Int
  | {- | "This location's shroud cannot be reduced." 'ShroudModifier's may still
    raise it, but the result is floored at the location's base value.
    'SetShroud' is deliberately exempt: it replaces the value outright rather
    than modifying it.
    -}
    ShroudCannotBeReduced
  | ShuffleIntoAnyDeckInsteadOfDiscard
  | ShuffleIntoDeckInsteadOfDiscard
  | SkillCannotBeIncreased SkillType
  | CalculatedSkillModifier {skillType :: SkillType, calculation :: GameCalculation}
  | SkillModifier {skillType :: SkillType, value :: Int}
  | SkillModifiersAffectOtherSkill SkillType SkillType
  | SkillTestAutomaticallySucceeds
  | SkillTestAutomaticallyFails
  | -- | If the test would succeed by at least this much, it fails instead
    AutomaticallyFailIfSucceedByAtLeast Int
  | SkillTestResultValueModifier Int
  | SkipMythosPhaseStep MythosPhaseStep
  | SlotCanBe SlotType SlotType
  | SpawnNonEliteAtConnectingInstead
  | StartingClues Int
  | StartingHand Int
  | StartingResources Int
  | StartsInEncounterDeck EncounterCard
  | StayInVictory
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
  | TreatFullyFloodedAsPartiallyFlooded
  | {- | On an INVESTIGATOR: their location counts as unflooded however flooded it is.
    What carries a boat's occupants over open water; the sibling of
    'TreatFullyFloodedAsPartiallyFlooded', which a diving suit grants.
    -}
    TreatLocationAsUnflooded
  | {- | On an INVESTIGATOR: where a card would have them "choose one", they resolve
    every option instead (via 'chooseOneAtATimeM'). Only read by the cards whose
    wording calls for it, so it does not leak into unrelated choices.
    -}
    MustResolveAllOptions
  | UIModifier UIModifier
  | BecomeHomunculusWhenDefeated
  | BecomeInvestigator InvestigatorId
  | DrawsEachEncounterCard
  | {- | When drawing encounter cards (e.g. the mythos draw), present this target
    to click instead of the encounter deck. The draw itself is unchanged.
    -}
    DrawEncounterCardsVia TargetMatcher
  deriving stock (Show, Eq, Ord, Data)

data UIModifier
  = Ethereal -- from Ethereal Form
  | Explosion -- from Dyanamite Blast
  | Locus -- from Prophesiae Profana
  | OnFire -- from Fire!, and anything else that should look like it is burning
  | ImportantToScenario Text
  | OverlayCheckmark {left :: Double, top :: Double} -- See The Stakeout for example
  | Rotated Int
  | Positioned {x :: Double, y :: Double} -- player-driven location drag offset (un-zoomed CSS px)
  | GridOffset {columnOffset :: Double, rowOffset :: Double} -- fractional location-grid cell offset
  | Oversized
  deriving stock (Show, Eq, Ord, Data)

instance IsLabel "combat" (Int -> ModifierType) where
  fromLabel = SkillModifier #combat

instance IsLabel "combat" (Integer -> ModifierType) where
  fromLabel = SkillModifier #combat . fromIntegral

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

instance IsLabel "damage" (Integer -> ModifierType) where
  fromLabel = DamageDealt . fromIntegral

instance IsLabel "noAction" ModifierType where
  fromLabel = ActionCostModifier (-1)

instance IsLabel "retaliate" ModifierType where
  fromLabel = AddKeyword Retaliate

instance IsLabel "alert" ModifierType where
  fromLabel = AddKeyword Alert

instance IsLabel "aloof" ModifierType where
  fromLabel = AddKeyword Aloof

instance IsLabel "hunter" ModifierType where
  fromLabel = AddKeyword Arkham.Keyword.Hunter

data Modifier = Modifier
  { modifierSource :: Source
  , modifierType :: ModifierType
  , modifierActiveDuringSetup :: Bool
  , modifierCard :: Maybe Card
  , modifierEffect :: Maybe EffectId
  {- ^ Stamped on by 'HasModifiersFor Effect', so the client can tell which
  modifiers a 'DisableEffect' can take back off.
  -}
  }
  deriving stock (Show, Eq, Ord, Data)

instance HasField "source" Modifier Source where
  getField = modifierSource

instance HasField "kind" Modifier ModifierType where
  getField = modifierType

instance HasField "activeDuringSetup" Modifier Bool where
  getField = modifierActiveDuringSetup

instance HasField "card" Modifier (Maybe Card) where
  getField = modifierCard

instance HasField "effect" Modifier (Maybe EffectId) where
  getField = modifierEffect

overModifierTypeM :: Monad m => (ModifierType -> m ModifierType) -> Modifier -> m Modifier
overModifierTypeM f m = f (modifierType m) <&> \mt -> m {modifierType = mt}

setActiveDuringSetup :: Modifier -> Modifier
setActiveDuringSetup m = m {modifierActiveDuringSetup = True}

-- | Drop the clues 'IgnoreCluesOnThis' hides from a raw clue count.
subtractIgnoredClues :: [ModifierType] -> Int -> Int
subtractIgnoredClues mods n = foldl' go n mods
 where
  go k = \case
    IgnoreCluesOnThis x -> max 0 (k - x)
    _ -> k

mconcat
  [ deriveToJSON defaultOptions ''ModifierType
  , [d|
      instance FromJSON ModifierType where
        parseJSON = withObject "ModifierType" \v -> do
          tag :: Text <- v .: "tag"
          case tag of
            "CanPlayUnderControlOf" -> do
              let parseRecord o = do
                    cmatch <- o .: "cardMatcher" <|> o .: "card"
                    imatch <- o .: "investigatorMatcher" <|> o .: "investigator"
                    pure $ CanPlayUnderControlOf cmatch imatch
              mContents <- v .:? "contents"
              case mContents of
                Just contents ->
                  (uncurry CanPlayUnderControlOf <$> parseJSON contents)
                    <|> withObject "CanPlayUnderControlOf" parseRecord contents
                    <|> (flip CanPlayUnderControlOf Anyone <$> parseJSON contents)
                Nothing -> parseRecord v
            "DrawAdditionalChaosTokens" -> do
              contents <- (Right <$> v .: "contents") <|> (Left <$> v .: "contents")
              case contents of
                Left n -> pure $ DrawAdditionalChaosTokens n ResolveOne
                Right (n, reveals) -> pure $ DrawAdditionalChaosTokens n reveals
            "MaxDamageTaken" -> do
              contents <- (Right <$> v .: "contents") <|> (Left <$> v .: "contents")
              case contents of
                Left n -> pure $ MaxDamageTaken AnyDamageEffect n
                Right (matcher, n) -> pure $ MaxDamageTaken matcher n
            "AsIfInHandForPlay" -> AsIfInHandFor ForPlay <$> v .: "contents"
            "SetSkillValue" -> do
              contents <-
                (Left <$> v .: "contents") <|> (Right <$> (SetSkillValue <$> v .: "skillType" <*> v .: "value"))
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
  , deriveToJSON (aesonOptions $ Just "modifier") ''Modifier
  , deriveJSON defaultOptions ''UIModifier
  , makePrisms ''ModifierType
  ]

-- Hand-written so a payload from before `effect` existed -- a recorded game, or
-- the debug client's `EffectModifiers` -- still parses.
instance FromJSON Modifier where
  parseJSON = withObject "Modifier" \o -> do
    modifierSource <- o .: "source"
    modifierType <- o .: "type"
    modifierActiveDuringSetup <- o .: "activeDuringSetup"
    modifierCard <- o .:? "card"
    modifierEffect <- o .:? "effect"
    pure Modifier {..}
