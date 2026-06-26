{-# OPTIONS_GHC -Wno-unused-record-wildcards -Wno-unused-imports -Wno-unused-matches -Wno-missing-signatures -Wno-orphans #-}
{-# LANGUAGE TypeAbstractions #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Investigator.Runner.Damage where


import Arkham.Ability as X hiding (PaidCost)
import Arkham.ChaosToken as X
import Arkham.ClassSymbol as X
import Arkham.Classes as X
import Arkham.ForMovement
import Arkham.Helpers.Investigator as X
import Arkham.Helpers.Message as X hiding (
  InvestigatorDamage,
  InvestigatorDefeated,
  InvestigatorResigned,
 )
import Arkham.Helpers.Query as X
import Arkham.Id as X
import Arkham.Investigator.Types as X
import Arkham.Name as X
import Arkham.Source as X
import Arkham.Stats as X
import Arkham.Target as X
import Arkham.Trait as X hiding (Cosmos, Cultist, ElderThing, Haunted)
import Data.Aeson (Result (..))
import Data.Aeson.KeyMap qualified as KeyMap

import Arkham.Action (Action)
import Arkham.Action qualified as Action
import Arkham.Action.Additional
import Arkham.Actions (actionsToList)
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (..))
import Arkham.Campaign.Option
import Arkham.CampaignLog
import Arkham.Campaigns.EdgeOfTheEarth.Seal
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Capability
import Arkham.Card
import Arkham.Card.PlayerCard
import Arkham.Card.Settings
import Arkham.Classes.HasGame
import Arkham.CommitRestriction
import Arkham.Constants
import Arkham.Cost qualified as Cost
import Arkham.Customization
import Arkham.Damage
import Arkham.DamageEffect
import Arkham.Deck qualified as Deck
import Arkham.DefeatedBy
import Arkham.Discard
import Arkham.Discover
import Arkham.Draw.Types
import Arkham.Enemy.Types qualified as Field
import Arkham.Event.Types (Field (..))
import Arkham.Fight.Types
import {-# SOURCE #-} Arkham.Game (asIfTurn, withoutCanModifiers)
import Arkham.Game.Settings (settingsStrictAsIfAt)
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers
import Arkham.Helpers.Ability (
  getAbilityLimit,
  getCanAffordUseWith,
  getCanPerformAbility,
  isForcedAbility,
 )
import Arkham.Helpers.Action (
  additionalActionCovers,
  canDo_,
  getActions,
  getActionsWith,
  getAdditionalActions,
  getCanAfford,
 )
import Arkham.Helpers.Card (
  cardIsFast',
  drawThisCardFrom,
  extendedCardMatch,
  getModifiedCardCost,
  passesLimits,
 )
import Arkham.Helpers.Cost (getCanAffordCost, getSpendableResources, hasSkillTestCost)
import Arkham.Helpers.Criteria (passesCriteria)
import Arkham.Helpers.Deck qualified as Deck
import Arkham.Helpers.Discover
import Arkham.Helpers.Game (withAlteredGame)
import Arkham.Helpers.Location (
  getCanMoveTo,
  getCanMoveToMatchingLocations,
  isDiscoveringLastClue,
  withLocationOf,
 )
import Arkham.Helpers.Log (hasCampaignOption)
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Playable (getIsPlayable, getIsPlayableWithResources, getPlayableCards)
import Arkham.Helpers.Ref (sourceToCard)
import Arkham.Helpers.SkillTest
import Arkham.Helpers.Slot (
  canPutIntoSlot,
  emptySlot,
  removeIfMatches,
  removeIfMatchesOnce,
  slotItems,
 )
import Arkham.Helpers.Source (getSourceController, sourceMatches, sourceTraits)
import Arkham.Helpers.Window (
  batchedTimings,
  checkAfter,
  checkWhen,
  checkWindows,
  frame,
  pushBatch,
  pushBatched,
  timings,
  windowMatches,
  wouldDo,
 )
import Arkham.Helpers.Window qualified as Helpers
import Arkham.History
import Arkham.I18n (cardNameVar, countVar, ikey', withI18n)
import Arkham.Investigate.Types
import {-# SOURCE #-} Arkham.Investigator
import Arkham.Investigator.Types qualified as Attrs
import Arkham.Key
import Arkham.Keyword (Keyword (Starting))
import Arkham.Location.Types (Field (..))
import Arkham.Matcher (
  basic,
  AssetMatcher (..),
  CardMatcher (..),
  EnemyMatcher (..),
  EventMatcher (..),
  ExtendedCardMatcher (..),
  ForPlay (..),
  InvestigatorMatcher (..),
  LocationMatcher (..),
  ScenarioMatcher (..),
  SourceMatcher (..),
  TreacheryMatcher (..),
  WindowMatcher (AnyWindow),
  assetControlledBy,
  assetIs,
  at_,
  cardIs,
  colocatedWith,
  enemyEngagedWith,
  inHandOf,
  locationWithInvestigator,
  mapOneOf,
  oneOf,
  orConnected,
  treacheryInThreatAreaOf,
  pattern AnyInPlayEnemy,
  pattern AssetWithAnyClues,
 )
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted (obtainCard, takeControlOfAsset)
import Arkham.Message.Lifted qualified as Lifted
import Arkham.Message.Lifted.Choose qualified as Choose
import Arkham.Message.Lifted.Move (moveTo, moveToEdit)
import Arkham.Modifier
import Arkham.Modifier qualified as Modifier
import Arkham.Movement
import Arkham.Phase
import Arkham.Placement
import Arkham.Plural
import Arkham.Prelude
import Arkham.Projection
import Arkham.ScenarioLogKey
import Arkham.Search hiding (drawnCardsL, foundCardsL)
import Arkham.Search qualified as Search
import Arkham.Skill.Types (Field (..))
import Arkham.SkillTest
import Arkham.Slot
import Arkham.Timing qualified as Timing
import Arkham.Token
import Arkham.Token qualified as Token
import Arkham.Tracing
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Treachery.Types (Field (..))
import Arkham.Window (Window (..), defaultWindows, mkAfter, mkWhen, mkWindow, primaryWindowTarget)
import Arkham.Window qualified as Window
import Arkham.Zone qualified as Zone
import Control.Lens (each, non, over, sumOf, _Just)
import Control.Monad.State.Strict (evalStateT, get, modify)
import Data.Data.Lens (biplate)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Monoid
import Data.Set qualified as Set
import Data.UUID (nil)


removeInvestigatorTokens :: Monad m => Token -> Int -> InvestigatorAttrs -> m InvestigatorAttrs
removeInvestigatorTokens token n a = case token of
  Damage | a.assignedHealthDamage > 0 ->
    let subtractFromAssigned = min a.assignedHealthDamage n
        subtractFromPool = max 0 (n - subtractFromAssigned)
     in pure
          $ a
          & (tokensL %~ subtractTokens token subtractFromPool)
          & (assignedHealthDamageL -~ subtractFromAssigned)
  Horror | a.assignedSanityDamage > 0 ->
    let subtractFromAssigned = min a.assignedSanityDamage n
        subtractFromPool = max 0 (n - subtractFromAssigned)
     in pure
          $ a
          & (tokensL %~ subtractTokens token subtractFromPool)
          & (assignedSanityDamageL -~ subtractFromAssigned)
  _ -> pure $ a & tokensL %~ subtractTokens token n

handleInvestigatorIsDefeated a@InvestigatorAttrs{..} source iid = do
  isLead <- (== iid) <$> getLead
  modifiedHealth <- field InvestigatorHealth (toId a)
  modifiedSanity <- field InvestigatorSanity (toId a)
  let
    defeatedByHorror = investigatorSanityDamage a >= modifiedSanity
    defeatedByDamage = investigatorHealthDamage a >= modifiedHealth
    defeatedBy = case (defeatedByHorror, defeatedByDamage) of
      (True, True) -> DefeatedByDamageAndHorror source
      (True, False) -> DefeatedByHorror source
      (False, True) -> DefeatedByDamage source
      (False, False) -> DefeatedByOther source
    physicalTrauma = if investigatorHealthDamage a >= modifiedHealth then 1 else 0
    mentalTrauma = if investigatorSanityDamage a >= modifiedSanity then 1 else 0
  windowMsg <- checkWindows [mkAfter $ Window.InvestigatorDefeated defeatedBy iid]
  killed <- hasModifier a KilledIfDefeated
  becomeHomunculus <- hasModifier a BecomeHomunculusWhenDefeated

  push windowMsg

  let chooseTrauma = physicalTrauma > 0 && mentalTrauma > 0

  when chooseTrauma do
    Choose.chooseOneM iid $ withI18n do
      Choose.labeled' "sufferPhysicalTrauma" $ Lifted.sufferPhysicalTrauma iid 1
      Choose.labeled' "sufferMentalTrauma" $ Lifted.sufferMentalTrauma iid 1

  pushAll
    $ CheckTrauma iid
    : [ChooseLeadInvestigator | isLead]
      <> [InvestigatorKilled (toSource a) iid | killed]
      <> [BecomeHomunculus iid | not killed && becomeHomunculus]
      <> [InvestigatorWhenEliminated (toSource a) iid Nothing]

  pure
    $ if chooseTrauma
      then a & (defeatedL .~ True) & (endedTurnL .~ True)
      else
        a
          & (defeatedL .~ True)
          & (endedTurnL .~ True)
          & (physicalTraumaL +~ physicalTrauma)
          & (mentalTraumaL +~ mentalTrauma)

handleInvestigatorEliminated a@InvestigatorAttrs{..} iid = do
  withLocationOf iid \lid -> do
    includeStory <- not <$> hasCampaignOption PlayersDoNotControlStoryAssetClues
    let storyWrapper = if includeStory then id else (<> AssetNonStory)
    Lifted.selectEach (storyWrapper $ assetControlledBy iid <> AssetWithAnyClues) \asset -> do
      assetClues <- field AssetClues asset
      push $ MoveTokens GameSource (AssetSource asset) (toTarget lid) Clue assetClues
    pushAll
      $ PlaceTokens (toSource a) (toTarget lid) Clue (investigatorClues a)
      : [PlaceKey (toTarget lid) k | k <- toList investigatorKeys]
  -- if this investigator was the target of an enemy attack we need to remove them
  let
    isAttackingThisInvestigator = \case
      EnemyAttack details -> any (isTarget iid) details.targets
      _ -> False
  let
    isNotEliminatedChoice = \case
      TargetLabel _ msgs -> none isAttackingThisInvestigator msgs
      _ -> True
  let removeEliminatedChoices = filter isNotEliminatedChoice
  lift $ mapQueue \case
    Ask who (ChooseOneAtATime choices) -> Ask who (ChooseOneAtATime $ removeEliminatedChoices choices)
    other -> other
  pure
    $ a
    & (tokensL %~ (removeAllTokens Clue . removeAllTokens Resource))
    & (keysL .~ mempty)
    & (handL .~ mempty)
    & (discardL .~ mempty)
    & (deckL .~ mempty)
    & (eliminatedL .~ True)
    & (placementL .~ Unplaced)

handlePlaceAdditionalDamage a@InvestigatorAttrs{..} target source damage horror = do
  push $ Msg.InvestigatorDamage (toId a) source damage horror
  pure a

handleInvestigatorDamageInvestigator a@InvestigatorAttrs{..} iid xid = do
  damage <- damageValueFor 1 iid DamageForInvestigator
  push $ InvestigatorAssignDamage xid (InvestigatorSource iid) DamageAny damage 0
  pure a

handleInvestigatorDamageEnemy a@InvestigatorAttrs{..} iid eid source = do
  cannotDamage <- hasModifier iid CannotDealDamage
  unless cannotDamage $ do
    damage <- damageValueFor 1 iid DamageForEnemy
    -- For a basic attack, attribute the damage to the investigator (for
    -- damage-dealt windows) while preserving the underlying ability source so
    -- gating modifiers like CannotBeDamagedByPlayerSourcesExcept
    -- (SourceIsAbility BasicAbility) still see the basic-ability exception.
    let
      source' =
        case source of
          AbilitySource s 100 -> UseAbilitySource iid s 100
          UseAbilitySource _ s 100 -> UseAbilitySource iid s 100
          _ -> source
    push $ Msg.DealDamage (EnemyTarget eid) $ attack source' damage
  pure a

handleCancelDamage a@InvestigatorAttrs{..} iid n = lift do
  withQueue_ \queue -> flip map queue $ \case
    Msg.InvestigatorDamage iid' s damage' horror' ->
      Msg.InvestigatorDamage iid' s (max 0 (damage' - n)) horror'
    InvestigatorDoAssignDamage iid' s t matcher' damage' horror' aa b ->
      InvestigatorDoAssignDamage iid' s t matcher' (max 0 (damage' - n)) horror' aa b
    other -> other
  pure a

handleCancelHorror a@InvestigatorAttrs{..} iid n = lift do
  withQueue_ \queue -> flip map queue $ \case
    Msg.InvestigatorDamage iid' s damage' horror' ->
      Msg.InvestigatorDamage iid' s damage' (max 0 (horror' - n))
    InvestigatorDoAssignDamage iid' s t matcher' damage' horror' aa b ->
      InvestigatorDoAssignDamage iid' s t matcher' damage' (max 0 (horror' - n)) aa b
    other -> other
  pure a

handleInvestigatorDirectDamage a@InvestigatorAttrs{..} iid source damage horror = do
  unless (investigatorDefeated || investigatorResigned) do
    mods <- getModifiers a
    let horrorToCancel =
          if any (`elem` mods) [CannotCancelHorror, CannotCancelHorrorFrom source]
            then 0
            else sum [n | WillCancelHorror n <- mods]
    let horror' = max 0 (horror - horrorToCancel)
    pushAll
      $ [ CheckWindows
            $ mkWhen (Window.WouldTakeDamageOrHorror source (toTarget a) damage horror')
            : [mkWhen (Window.WouldTakeDamage source (toTarget a) damage DamageDirect) | damage > 0]
              <> [mkWhen (Window.WouldTakeHorror source (toTarget a) horror') | horror' > 0]
        | damage > 0 || horror' > 0
        ]
      <> [ InvestigatorDoAssignDamage
             iid
             source
             DamageAny
             (AssetWithModifier CanBeAssignedDirectDamage)
             damage
             horror'
             []
             []
         ]
  pure a

handleInvestigatorAssignDamage a@InvestigatorAttrs{..} iid source strategy damage horror = do
  unless (investigatorDefeated || investigatorResigned) do
    mods <- getModifiers a
    if TreatAllDamageAsDirect `elem` mods
      then push $ InvestigatorDirectDamage iid source damage horror
      else do
        let horrorToCancel =
              if any (`elem` mods) [CannotCancelHorror, CannotCancelHorrorFrom source]
                then 0
                else sum [n | WillCancelHorror n <- mods]
        let horror' = max 0 (horror - horrorToCancel)
        pushAll
          $ [ CheckWindows
                $ mkWhen (Window.WouldTakeDamageOrHorror source (toTarget a) damage horror')
                : [mkWhen (Window.WouldTakeDamage source (toTarget a) damage strategy) | damage > 0]
                  <> [mkWhen (Window.WouldTakeHorror source (toTarget a) horror') | horror' > 0]
            | damage > 0 || horror' > 0
            ]
          <> [InvestigatorDoAssignDamage iid source strategy AnyAsset damage horror' [] []]
  pure a

finalizeDeferredDamageAssignment a@InvestigatorAttrs{..} iid source damageStrategy damageTargets horrorTargets = do
  let
    assetDamageMap = Map.fromListWith (+) [(aid, 1 :: Int) | AssetTarget aid <- damageTargets]
    assetHorrorMap = Map.fromListWith (+) [(aid, 1 :: Int) | AssetTarget aid <- horrorTargets]
    affectedAssets = nub $ Map.keys assetDamageMap <> Map.keys assetHorrorMap
    placementMessages =
      [ Msg.AssignAssetDamageWithCheck aid source damage horror False
      | aid <- affectedAssets
      , let damage = findWithDefault 0 aid assetDamageMap
      , let horror = findWithDefault 0 aid assetHorrorMap
      , damage > 0 || horror > 0
      ]
    damageEffect = case source of
      EnemyAttackSource _ -> AttackDamageEffect
      _ -> NonAttackDamageEffect
    damageMap = frequencies damageTargets
    horrorMap = frequencies horrorTargets
    placedWindows =
      [ Window.PlacedDamage source target damage
      | (target, damage) <- mapToList damageMap
      ]
        <> [ Window.PlacedHorror source target horror
           | (target, horror) <- mapToList horrorMap
           ]
    checkAssets = nub $ keys horrorMap <> keys damageMap
  whenPlacedWindowMsg <- checkWindows $ map mkWhen placedWindows
  whenAssignedWindowMsg <- checkWhen $ Window.AssignedHorror source iid horrorTargets
  let assignedHorror = count (== toTarget a) horrorTargets
  when
    ( (damageStrategy == DamageFromHastur)
        && (toTarget a `elem` horrorTargets)
        && (investigatorSanityDamage a + assignedHorror > investigatorSanity)
    )
    do
      push $ InvestigatorDirectDamage iid source 1 0

  let totalDamage = length damageTargets
  let totalHorror = length horrorTargets

  pushAll
    $ placementMessages
      <> [ whenPlacedWindowMsg
         , CheckWindows
             $ [mkWhen (Window.TakeDamage source damageEffect (toTarget iid) totalDamage) | totalDamage > 0]
             <> [mkWhen (Window.TakeHorror source (toTarget iid) totalHorror) | totalHorror > 0]
             <> [ mkWhen (Window.DealtDamage source damageEffect target damage)
                | target <- nub damageTargets
                , let damage = count (== target) damageTargets
                ]
             <> [ mkWhen (Window.DealtHorror source target horror)
                | target <- nub horrorTargets
                , let horror = count (== target) horrorTargets
                ]
         ]
      <> [whenAssignedWindowMsg | notNull horrorTargets]
      <> [CheckDefeated source (toTarget aid) | aid <- checkAssets]
      <> [ CheckWindows
             $ map mkAfter placedWindows
             <> [mkAfter (Window.TakeDamage source damageEffect (toTarget iid) totalDamage) | totalDamage > 0]
             <> [mkAfter (Window.TakeHorror source (toTarget iid) totalHorror) | totalHorror > 0]
             <> [ mkAfter (Window.DealtDamage source damageEffect target damage)
                | target <- nub damageTargets
                , let damage = count (== target) damageTargets
                ]
             <> [ mkAfter (Window.DealtHorror source target horror)
                | target <- nub horrorTargets
                , let horror = count (== target) horrorTargets
                ]
             <> [mkAfter (Window.AssignedHorror source iid horrorTargets) | notNull horrorTargets]
         ]
  pure a

finalizeDamageAssignment a@InvestigatorAttrs{..} iid source damageStrategy damageTargets horrorTargets = do
  let
    damageEffect = case source of
      EnemyAttackSource _ -> AttackDamageEffect
      _ -> NonAttackDamageEffect
    damageMap = frequencies damageTargets
    horrorMap = frequencies horrorTargets
    placedWindows =
      [ Window.PlacedDamage source target damage
      | (target, damage) <- mapToList damageMap
      ]
        <> [ Window.PlacedHorror source target horror
           | (target, horror) <- mapToList horrorMap
           ]
    checkAssets = nub $ keys horrorMap <> keys damageMap
  whenPlacedWindowMsg <- checkWindows $ map mkWhen placedWindows
  whenAssignedWindowMsg <- checkWhen $ Window.AssignedHorror source iid horrorTargets
  let assignedHorror = count (== toTarget a) horrorTargets
  when
    ( (damageStrategy == DamageFromHastur)
        && (toTarget a `elem` horrorTargets)
        && (investigatorSanityDamage a + assignedHorror > investigatorSanity)
    )
    do
      push $ InvestigatorDirectDamage iid source 1 0

  let totalDamage = length damageTargets
  let totalHorror = length horrorTargets

  pushAll
    $ whenPlacedWindowMsg
    : [ CheckWindows
          $ [mkWhen (Window.TakeDamage source damageEffect (toTarget iid) totalDamage) | totalDamage > 0]
          <> [mkWhen (Window.TakeHorror source (toTarget iid) totalHorror) | totalHorror > 0]
          <> [ mkWhen (Window.DealtDamage source damageEffect target damage)
             | target <- nub damageTargets
             , let damage = count (== target) damageTargets
             ]
          <> [ mkWhen (Window.DealtHorror source target horror)
             | target <- nub horrorTargets
             , let horror = count (== target) horrorTargets
             ]
      ]
      <> [whenAssignedWindowMsg | notNull horrorTargets]
      <> [CheckDefeated source (toTarget aid) | aid <- checkAssets]
      <> [ CheckWindows
             $ map mkAfter placedWindows
             <> [mkAfter (Window.TakeDamage source damageEffect (toTarget iid) totalDamage) | totalDamage > 0]
             <> [mkAfter (Window.TakeHorror source (toTarget iid) totalHorror) | totalHorror > 0]
             <> [ mkAfter (Window.DealtDamage source damageEffect target damage)
                | target <- nub damageTargets
                , let damage = count (== target) damageTargets
                ]
             <> [ mkAfter (Window.DealtHorror source target horror)
                | target <- nub horrorTargets
                , let horror = count (== target) horrorTargets
                ]
             <> [mkAfter (Window.AssignedHorror source iid horrorTargets) | notNull horrorTargets]
         ]
  pure a

assignHealthDamageEvenly a@InvestigatorAttrs{..} iid source matcher health damageTargets horrorTargets = do
  healthDamageableAssets <-
    toList <$> getHealthDamageableAssets iid matcher source health damageTargets horrorTargets
  healthDamageableInvestigators <- select $ InvestigatorCanBeAssignedDamageBy iid

  mustBeDamagedFirstBeforeInvestigator <- forMaybeM healthDamageableAssets $ \aid -> do
    mods <- getModifiers aid
    let n = sum [x | NonDirectDamageMustBeAssignToThisN x <- mods]
    let mustAssignRemaining = n > 0 && health <= n && count (== toTarget aid) damageTargets < n
    pure $ guard (NonDirectDamageMustBeAssignToThisFirst `elem` mods || mustAssignRemaining) $> aid

  let
    onlyAssets = any (`elem` mustBeDamagedFirstBeforeInvestigator) healthDamageableAssets
    allowedDamage =
      findFewestOccurrences
        damageTargets
        ( map toTarget healthDamageableAssets
            <> [InvestigatorTarget iid | not onlyAssets]
            <> (if not onlyAssets then map toTarget healthDamageableInvestigators else [])
        )
    assignRestOfHealthDamage =
      InvestigatorDoAssignDamage
        investigatorId
        source
        DamageEvenly
        matcher
        (health - 1)
        0
    -- N.B. we have to add to the end of targets to handle the drop logic
    damageAsset aid =
      ComponentLabel
        (AssetComponent aid DamageToken)
        [ Msg.AssignAssetDamageWithCheck aid source 1 0 False
        , assignRestOfHealthDamage (damageTargets <> [AssetTarget aid]) mempty
        ]
    damageInvestigator iid' =
      ComponentLabel
        (InvestigatorComponent iid' DamageToken)
        [ Msg.InvestigatorDamage iid' source 1 0
        , assignRestOfHealthDamage
            (damageTargets <> [InvestigatorTarget iid'])
            mempty
        ]
    healthDamageMessages =
      [ damageInvestigator investigatorId
      | InvestigatorTarget investigatorId `elem` allowedDamage
      ]
        <> map damageAsset (filter ((`elem` allowedDamage) . toTarget) healthDamageableAssets)
        <> map damageInvestigator (filter ((`elem` allowedDamage) . toTarget) healthDamageableInvestigators)
  player <- getPlayer iid
  push $ chooseOne player healthDamageMessages
  pure a

assignHorrorEvenly a@InvestigatorAttrs{..} iid source matcher sanity damageTargets horrorTargets = do
  sanityDamageableAssets <-
    toList <$> getSanityDamageableAssets iid matcher source sanity damageTargets horrorTargets
  sanityDamageableInvestigators <- select $ InvestigatorCanBeAssignedHorrorBy iid
  mustBeDamagedFirstBeforeInvestigator <-
    select
      ( AssetCanBeAssignedHorrorBy iid
          <> AssetWithModifier NonDirectHorrorMustBeAssignToThisFirst
          <> AssetCanBeDamagedBySource source
      )
  let
    onlyAssets = any (`elem` mustBeDamagedFirstBeforeInvestigator) sanityDamageableAssets
    allowedDamage =
      findFewestOccurrences
        horrorTargets
        $ map toTarget sanityDamageableAssets
        <> [InvestigatorTarget iid | not onlyAssets]
        <> (if not onlyAssets then map toTarget sanityDamageableInvestigators else [])
    assignRestOfSanityDamage =
      InvestigatorDoAssignDamage
        investigatorId
        source
        DamageEvenly
        matcher
        0
        (sanity - 1)
    -- N.B. we have to add to the end of targets to handle the drop logic
    damageAsset aid =
      ComponentLabel
        (AssetComponent aid HorrorToken)
        [ Msg.AssignAssetDamageWithCheck aid source 0 1 False
        , assignRestOfSanityDamage mempty (horrorTargets <> [AssetTarget aid])
        ]
    damageInvestigator iid' =
      ComponentLabel
        (InvestigatorComponent iid' HorrorToken)
        [ Msg.InvestigatorDamage iid' source 0 1
        , assignRestOfSanityDamage
            mempty
            (horrorTargets <> [InvestigatorTarget iid'])
        ]
    sanityDamageMessages =
      [ damageInvestigator investigatorId
      | InvestigatorTarget investigatorId `elem` allowedDamage
      ]
        <> map damageAsset (filter ((`elem` allowedDamage) . toTarget) sanityDamageableAssets)
        <> map damageInvestigator (filter ((`elem` allowedDamage) . toTarget) sanityDamageableInvestigators)
  player <- getPlayer iid
  push $ chooseOne player sanityDamageMessages
  pure a

assignDamageEvenlyUnsupported a@InvestigatorAttrs{..} iid = do
  error "DamageEvenly only works with just horror or just damage, but not both"

assignDamageToSingleTarget a@InvestigatorAttrs{..} iid source matcher health sanity damageTargets horrorTargets = do
  healthDamageableAssets <-
    getHealthDamageableAssets iid matcher source health damageTargets horrorTargets
  healthDamageableInvestigators <- select $ InvestigatorCanBeAssignedDamageBy iid
  sanityDamageableAssets <-
    getSanityDamageableAssets iid matcher source sanity damageTargets horrorTargets
  sanityDamageableInvestigators <- select $ InvestigatorCanBeAssignedHorrorBy iid

  mustBeAssignedDamageFirstBeforeInvestigator <- forMaybeM (toList healthDamageableAssets) \aid -> do
    mods <- getModifiers aid
    let n = sum [x | NonDirectDamageMustBeAssignToThisN x <- mods]
    let mustAssignRemaining = n > 0 && health <= n && count (== toTarget aid) damageTargets < n
    pure $ guard (NonDirectDamageMustBeAssignToThisFirst `elem` mods || mustAssignRemaining) $> aid

  mustBeAssignedHorrorFirstBeforeInvestigator <-
    select
      $ AssetCanBeAssignedHorrorBy iid
      <> AssetWithModifier NonDirectHorrorMustBeAssignToThisFirst
      <> AssetCanBeDamagedBySource source

  let
    damageableAssets = toList $ healthDamageableAssets `union` sanityDamageableAssets
    damageableInvestigators = nub $ healthDamageableInvestigators <> sanityDamageableInvestigators
    onlyAssets =
      (any (`elem` mustBeAssignedDamageFirstBeforeInvestigator) damageableAssets && health > 0)
        || (any (`elem` mustBeAssignedHorrorFirstBeforeInvestigator) damageableAssets && sanity > 0)
    continue h s t =
      InvestigatorDoAssignDamage
        iid
        source
        SingleTarget
        matcher
        (max 0 $ health - h)
        (max 0 $ sanity - s)
        (damageTargets <> [t | h > 0])
        (horrorTargets <> [t | s > 0])
    toAssetMessage (asset, (h, s)) =
      TargetLabel
        (AssetTarget asset)
        [ Msg.AssignAssetDamageWithCheck asset source (min h health) (min s sanity) False
        , continue h s (AssetTarget asset)
        ]
    toInvestigatorMessage (hank, (h, s)) =
      TargetLabel
        (InvestigatorTarget hank)
        [ Msg.InvestigatorDamage hank source (min h health) (min s sanity)
        , continue h s (InvestigatorTarget hank)
        ]
  assetsWithCounts <- for damageableAssets $ \asset -> do
    health' <- fieldMap AssetRemainingHealth (fromMaybe 0) asset
    sanity' <- fieldMap AssetRemainingSanity (fromMaybe 0) asset
    pure (asset, (health', sanity'))
  investigatorsWithCounts <- for damageableInvestigators $ \hank -> do
    health' <- field InvestigatorRemainingHealth hank
    sanity' <- field InvestigatorRemainingSanity hank
    pure (hank, (health', sanity'))

  player <- getPlayer iid
  push
    $ chooseOne player
    $ [ targetLabel
          a
          [ Msg.InvestigatorDamage investigatorId source health sanity
          , continue health sanity (toTarget a)
          ]
      | not onlyAssets
      ]
    <> map toAssetMessage assetsWithCounts
    <> (if not onlyAssets then map toInvestigatorMessage investigatorsWithCounts else [])
  pure a

-- | Header shown while the player assigns damage/horror, so they can see the
-- total amounts that still need to be applied.
assignDamageTotalsLabel :: Int -> Int -> Text
assignDamageTotalsLabel health sanity = case (health, sanity) of
  (h, 0) -> "Assign " <> tshow h <> " damage"
  (0, s) -> "Assign " <> tshow s <> " horror"
  (h, s) -> "Assign " <> tshow h <> " damage and " <> tshow s <> " horror"

assignDamageDivided a@InvestigatorAttrs{..} iid source strategy matcher health sanity damageTargets horrorTargets = do
  healthDamageMessages <-
    if health > 0
      then do
        healthDamageableAssets <-
          toList <$> getHealthDamageableAssets iid matcher source health damageTargets horrorTargets
        healthDamageableInvestigators <- select $ InvestigatorCanBeAssignedDamageBy iid
        let
          assignRestOfHealthDamage rest =
            InvestigatorDoAssignDamage investigatorId source strategy matcher rest sanity
          -- Accumulate the soaked damage on the asset now (for display + to cap how
          -- much more it can take), but defer the actual token placement to the end
          -- so all of a source's damage lands at once -- mirroring how investigator
          -- damage is deferred via assignedHealthDamage.
          damageAsset aid applyAll =
            AssetDamageLabel
              aid
              [ Msg.AssignAssetDamageDeferred aid source (if applyAll then health else 1) 0
              , assignRestOfHealthDamage
                  (if applyAll then 0 else health - 1)
                  ((if applyAll then replicate health else pure) (AssetTarget aid) <> damageTargets)
                  horrorTargets
              ]
          damageInvestigator iid' applyAll =
            DamageLabel
              iid'
              [ Msg.InvestigatorDamage iid' source (if applyAll then health else 1) 0
              , assignRestOfHealthDamage
                  (if applyAll then 0 else health - 1)
                  ((if applyAll then replicate health else pure) (toTarget iid') <> damageTargets)
                  horrorTargets
              ]
        let
          go = \case
            AmongInvestigators imatcher -> do
              iids <- select imatcher
              -- The damage is dealt to the matched investigators "divided as they
              -- wish" and is *not* direct, so each point can also be soaked by an
              -- asset controlled by one of those investigators. Offer one point at a
              -- time across every matched investigator and their damageable assets.
              soakAssets <-
                fmap (toList . mconcat)
                  $ for iids \i -> getHealthDamageableAssets i matcher source health damageTargets horrorTargets
              pure $ case (iids, soakAssets) of
                ([], _) -> []
                ([iid'], []) -> [damageInvestigator iid' True]
                _ ->
                  [damageInvestigator iid' False | iid' <- iids]
                    <> [damageAsset aid False | aid <- soakAssets]
            DamageAssetsFirst amatcher -> do
              matchingAssets <- select $ mapOneOf AssetWithId healthDamageableAssets <> amatcher
              healthDamageableAssets' <-
                mapMaybe (\(x, mb) -> (x,) <$> mb) <$> forToSnd matchingAssets (field AssetRemainingHealth)
              let
                targetCount =
                  if null healthDamageableAssets'
                    then 1 + length healthDamageableInvestigators
                    else length healthDamageableAssets'
                applyAll = targetCount == 1
              pure
                $ [damageInvestigator iid applyAll | null healthDamageableAssets']
                <> map (`damageInvestigator` applyAll) healthDamageableInvestigators
                <> map (\(x, n) -> damageAsset x (n >= health && applyAll)) healthDamageableAssets'
            HorrorAssetsFirst _ -> do
              let
                targetCount =
                  if null healthDamageableAssets
                    then 1 + length healthDamageableInvestigators
                    else length healthDamageableAssets
                applyAll = targetCount == 1
              pure
                $ [damageInvestigator iid applyAll]
                <> map (`damageInvestigator` applyAll) healthDamageableInvestigators
                <> map (`damageAsset` applyAll) healthDamageableAssets
            DamageDirect -> pure [damageInvestigator iid True]
            DamageFromHastur -> go DamageAny
            DamageAnyDeferred -> go DamageAny
            DamageAny -> do
              healthDamageableAssets' <-
                mapMaybe (\(x, mb) -> (x,) <$> mb) <$> forToSnd healthDamageableAssets (field AssetRemainingHealth)
              mustBeAssignedDamage <-
                healthDamageableAssets' & filterM \(aid, _) -> do
                  mods <- getModifiers aid
                  let n = sum [x | NonDirectDamageMustBeAssignToThisN x <- mods]
                  pure $ n > 0 && health <= n && count (== toTarget aid) damageTargets < n

              mustBeAssignedDamageFirstBeforeInvestigator <-
                healthDamageableAssets' & filterM \(aid, _) -> do
                  mods <- getModifiers aid
                  pure $ NonDirectDamageMustBeAssignToThisFirst `elem` mods

              let
                targetCount =
                  if
                    | null mustBeAssignedDamage && null mustBeAssignedDamageFirstBeforeInvestigator ->
                        1 + length healthDamageableAssets' + length healthDamageableInvestigators
                    | null mustBeAssignedDamage -> length healthDamageableAssets' + length healthDamageableInvestigators
                    | otherwise -> length mustBeAssignedDamage

              let applyAll = null mustBeAssignedDamage && targetCount == 1

              pure
                $ [ damageInvestigator iid applyAll
                  | null mustBeAssignedDamage && null mustBeAssignedDamageFirstBeforeInvestigator
                  ]
                <> map
                  (\(x, n) -> damageAsset x (n >= health && applyAll))
                  (if null mustBeAssignedDamage then healthDamageableAssets' else mustBeAssignedDamage)
                <> map
                  (`damageInvestigator` applyAll)
                  (guard (null mustBeAssignedDamage) *> healthDamageableInvestigators)
            DamageFirst def -> do
              validAssets <-
                List.intersect healthDamageableAssets
                  <$> select (matcher <> assetControlledBy iid <> assetIs def)
              pure
                $ if null validAssets
                  then
                    damageInvestigator iid False
                      : map (`damageAsset` False) healthDamageableAssets
                        <> map (`damageInvestigator` False) healthDamageableInvestigators
                  else map (`damageAsset` False) validAssets
            SingleTarget -> error "handled elsewhere"
            DamageEvenly -> error "handled elsewhere"
        go strategy
      else pure []
  sanityDamageMessages <-
    if sanity > 0
      then do
        sanityDamageableAssets <-
          toList <$> getSanityDamageableAssets iid matcher source sanity damageTargets horrorTargets
        sanityDamageableInvestigators <- select $ InvestigatorCanBeAssignedHorrorBy iid
        let
          assignRestOfSanityDamage rest =
            InvestigatorDoAssignDamage investigatorId source strategy matcher health rest
          damageInvestigator iid' applyAll =
            HorrorLabel
              iid'
              [ Msg.InvestigatorDamage iid' source 0 (if applyAll then sanity else 1)
              , assignRestOfSanityDamage
                  (if applyAll then 0 else sanity - 1)
                  damageTargets
                  ((if applyAll then replicate sanity else pure) (toTarget iid') <> horrorTargets)
              ]

          auxiliaryDamageInvestigator iid' =
            AuxiliaryHorrorLabel
              iid'
              [ Msg.InvestigatorDamage iid' source 0 sanity
              , assignRestOfSanityDamage 0 damageTargets
                  $ replicate sanity (toTarget iid')
                  <> horrorTargets
              ]
          -- See the health branch: accumulate soaked horror now (display + cap),
          -- defer the actual token placement to the end so it lands all at once.
          damageAsset aid applyAll =
            AssetHorrorLabel
              aid
              [ Msg.AssignAssetDamageDeferred aid source 0 (if applyAll then sanity else 1)
              , assignRestOfSanityDamage
                  (if applyAll then 0 else sanity - 1)
                  damageTargets
                  ((if applyAll then replicate sanity else pure) (toTarget aid) <> horrorTargets)
              ]
        let
          go = \case
            AmongInvestigators imatcher -> do
              iids <- select imatcher
              -- See the health branch: horror dealt "divided as they wish" is not
              -- direct, so allow soaking onto the matched investigators' assets.
              soakAssets <-
                fmap (toList . mconcat)
                  $ for iids \i -> getSanityDamageableAssets i matcher source sanity damageTargets horrorTargets
              pure $ case (iids, soakAssets) of
                ([], _) -> []
                ([iid'], []) -> [damageInvestigator iid' True]
                _ ->
                  [damageInvestigator iid' False | iid' <- iids]
                    <> [damageAsset aid False | aid <- soakAssets]
            DamageAssetsFirst _ -> do
              sanityDamageableAssets' <-
                mapMaybe (\(x, mb) -> (x,) <$> mb) <$> forToSnd sanityDamageableAssets (field AssetRemainingSanity)
              let
                targetCount =
                  if null sanityDamageableAssets'
                    then 1 + length sanityDamageableInvestigators
                    else length sanityDamageableAssets'
                applyAll = targetCount == 1

              pure $ [damageInvestigator iid applyAll]
                <> map (\(x, n) -> damageAsset x (n >= sanity && applyAll)) sanityDamageableAssets'
                <> map (`damageInvestigator` applyAll) sanityDamageableInvestigators
            HorrorAssetsFirst amatcher -> do
              sanityDamageableAssets' <- select $ mapOneOf AssetWithId sanityDamageableAssets <> amatcher
              let
                targetCount =
                  if null sanityDamageableAssets'
                    then 1 + length sanityDamageableInvestigators
                    else length sanityDamageableAssets'
                applyAll = targetCount == 1

              pure $ [damageInvestigator iid applyAll | null sanityDamageableAssets']
                <> map (`damageAsset` applyAll) sanityDamageableAssets'
                <> map (`damageInvestigator` applyAll) sanityDamageableInvestigators
            DamageDirect -> pure [damageInvestigator iid True]
            DamageFromHastur -> go DamageAny
            DamageAnyDeferred -> go DamageAny
            DamageAny -> do
              mustBeAssignedHorrorFirstBeforeInvestigator <- flip filterM sanityDamageableAssets \aid -> do
                mods <- getModifiers aid
                pure $ NonDirectHorrorMustBeAssignToThisFirst `elem` mods
              let
                targetCount =
                  if null mustBeAssignedHorrorFirstBeforeInvestigator
                    then 1 + length sanityDamageableAssets + length sanityDamageableInvestigators
                    else length sanityDamageableAssets + length sanityDamageableInvestigators

              let applyAll = targetCount == 1

              pure $ [damageInvestigator iid applyAll | null mustBeAssignedHorrorFirstBeforeInvestigator]
                <> map (`damageAsset` applyAll) sanityDamageableAssets
                <> map (`damageInvestigator` applyAll) sanityDamageableInvestigators
                <> [ auxiliaryDamageInvestigator iid
                   | null mustBeAssignedHorrorFirstBeforeInvestigator && not applyAll && sanity > 1
                   ]
            DamageFirst def -> do
              validAssets <-
                List.intersect sanityDamageableAssets
                  <$> select (matcher <> assetControlledBy iid <> assetIs def)
              pure
                $ if null validAssets
                  then
                    damageInvestigator iid False
                      : map (`damageAsset` False) sanityDamageableAssets
                        <> map (`damageInvestigator` False) sanityDamageableInvestigators
                  else map (`damageAsset` False) validAssets
            SingleTarget -> error "handled elsewhere"
            DamageEvenly -> error "handled elsewhere"
        go strategy
      else pure []
  player <- getPlayer iid
  -- Wrap with the damage source so the client highlights it as the actor
  -- (yellow source-highlight), and with the totals label for the token counts.
  push
    $ questionWithSource source player
    $ QuestionLabel (assignDamageTotalsLabel health sanity) Nothing
    $ ChooseOne
    $ healthDamageMessages <> sanityDamageMessages
  pure a

handleDrivenInsane a@InvestigatorAttrs{..} iid = do
  pure $ a & mentalTraumaL .~ investigatorSanity & drivenInsaneL .~ True & defeatedL .~ True

handleCheckDefeated a@InvestigatorAttrs{..} source = do
  facingDefeat <- getFacingDefeat a
  if facingDefeat
    then do
      modifiedHealth <- field InvestigatorHealth (toId a)
      modifiedSanity <- field InvestigatorSanity (toId a)
      let
        defeatedByHorror =
          investigatorSanityDamage a
            + investigatorAssignedSanityDamage
            >= modifiedSanity
        defeatedByDamage =
          investigatorHealthDamage a
            + investigatorAssignedHealthDamage
            >= modifiedHealth
        defeatedBy = case (defeatedByHorror, defeatedByDamage) of
          (True, True) -> DefeatedByDamageAndHorror source
          (True, False) -> DefeatedByHorror source
          (False, True) -> DefeatedByDamage source
          (False, False) -> DefeatedByOther source
      windowMsg <- checkWindows [mkWhen $ Window.InvestigatorWouldBeDefeated defeatedBy (toId a)]
      pushAll
        [ windowMsg
        , AssignDamage (InvestigatorTarget $ toId a)
        , InvestigatorWhenDefeated source investigatorId
        ]
    else push $ AssignDamage (InvestigatorTarget $ toId a)

  pure a

handleAssignDamage a@InvestigatorAttrs{..} target = do
  push $ AssignedDamage target investigatorAssignedHealthDamage investigatorAssignedSanityDamage
  pure
    $ a
    & tokensL
    %~ ( addTokens Token.Damage investigatorAssignedHealthDamage
           . addTokens Horror investigatorAssignedSanityDamage
       )
    & (assignedHealthDamageL .~ 0)
    & (assignedSanityDamageL .~ 0)
    & (unhealedHorrorThisRoundL +~ investigatorAssignedSanityDamage)

handleCancelAssignedDamage a@InvestigatorAttrs{..} target damageReduction horrorReduction = do
  pure
    $ a
    & (assignedHealthDamageL %~ max 0 . subtract damageReduction)
    & (assignedSanityDamageL %~ max 0 . subtract horrorReduction)

handleApplyHealing a@InvestigatorAttrs{..} source msg = do
  cannotHealHorror <- sourcePerformerHasModifier source CannotHealHorror
  cannotHealDamage <- sourcePerformerHasModifier source CannotHealDamage
  let health = if cannotHealDamage then 0 else findWithDefault 0 source investigatorAssignedHealthHeal
  let sanity = if cannotHealHorror then 0 else findWithDefault 0 source investigatorAssignedSanityHeal
  let totalSanity = sanity + investigatorHorrorHealed

  when (health > 0 || sanity > 0) do
    pushM
      $ checkWindows
      $ [mkWhen (Window.Healed DamageType (toTarget a) source health) | health > 0]
      <> [mkWhen (Window.Healed HorrorType (toTarget a) source totalSanity) | totalSanity > 0]
    push $ Do msg
  pure a

handleDoApplyHealing a@InvestigatorAttrs{..} source = do
  cannotHealHorror <- sourcePerformerHasModifier source CannotHealHorror
  cannotHealDamage <- sourcePerformerHasModifier source CannotHealDamage
  let health = if cannotHealDamage then 0 else findWithDefault 0 source investigatorAssignedHealthHeal
  let sanity = if cannotHealHorror then 0 else findWithDefault 0 source investigatorAssignedSanityHeal
  let totalSanity = sanity + investigatorHorrorHealed

  let overHealDamage = max 0 (health - a.healthDamage - a.assignedHealthDamage)
  let overHealSanity = max 0 (totalSanity - a.sanityDamage - a.assignedSanityDamage)

  pushWhen (overHealDamage > 0) $ ExcessHealDamage a.id source overHealDamage
  pushWhen (overHealSanity > 0) $ ExcessHealHorror a.id source overHealSanity

  when (health > 0 || totalSanity > 0) do
    pushM
      $ checkWindows
      $ [mkAfter (Window.Healed DamageType (toTarget a) source health) | health > 0]
      <> [mkAfter (Window.Healed HorrorType (toTarget a) source sanity) | sanity > 0]
    push $ AssignedHealing (toTarget a)

  let trueHealth = min health (a.healthDamage + a.assignedHealthDamage)
  -- horror healed was already applied so we ignore it here
  let trueSanity = min sanity (a.sanityDamage + a.assignedSanityDamage)

  a' <-
    if trueHealth > 0
      then removeInvestigatorTokens #damage trueHealth a
      else pure a
  a'' <-
    if trueSanity > 0
      then removeInvestigatorTokens #horror trueSanity a'
      else pure a'

  pure
    $ a''
    & (unhealedHorrorThisRoundL %~ min 0 . subtract sanity)
    & (assignedHealthHealL %~ deleteMap source)
    & (assignedSanityHealL %~ deleteMap source)
    & (horrorHealedL .~ 0)

handleHealDamage a@InvestigatorAttrs{..} iid source amount' msg = do
  mods <- getModifiers a
  cannotHealDamage <- sourcePerformerHasModifier source CannotHealDamage
  let canHealAtFullSources = [sourceMatcher | CanHealAtFull sourceMatcher DamageType <- mods]
  canHealAtFull <-
    if null canHealAtFullSources
      then pure False
      else sourceMatches source (mconcat canHealAtFullSources)
  unless cannotHealDamage do
    let n = sum [x | HealingTaken x <- mods]
    let amount = amount' + n
    whenWindow <- checkWindows [mkWhen $ Window.Healed DamageType (toTarget a) source amount]
    dmgTreacheries <-
      selectWithField TreacheryCard $ treacheryInThreatAreaOf iid <> TreacheryWithModifier IsPointOfDamage
    if null dmgTreacheries
      then do
        let remainingDamage =
              (investigatorHealthDamage a + investigatorAssignedHealthDamage)
                - sum (toList investigatorAssignedHealthHeal)
        when (remainingDamage > 0 || canHealAtFull) do
          pushAll [whenWindow, Do msg]
      else do
        player <- getPlayer iid
        push
          $ chooseOne player
          $ [ Label
                (withI18n $ cardNameVar c $ ikey' "label.healName")
                $ toDiscardBy iid source t
                : [HealDamage (InvestigatorTarget iid) source (amount - 1) | amount - 1 > 0]
            | (t, c) <- dmgTreacheries
            ]
          <> [Label "$label.healRemainingDamageNormally" [whenWindow, Do msg] | investigatorHealthDamage a > 0]
  pure a

handleDoHealDamage a@InvestigatorAttrs{..} iid source amount = do
  cannotHealDamage <- sourcePerformerHasModifier source CannotHealDamage
  unless cannotHealDamage do
    pushAll [HealDamageDelayed (InvestigatorTarget iid) source amount, ApplyHealing source]
  pure a

handleHealDamageDelayed a@InvestigatorAttrs{..} source n = do
  cannotHealDamage <- sourcePerformerHasModifier source CannotHealDamage
  if cannotHealDamage
    then pure a
    else pure $ a & assignedHealthHealL %~ insertWith (+) source n

handleHealHorrorWithAdditional a@InvestigatorAttrs{..} iid amount = do
  -- exists to have no callbacks, and to be resolved with AdditionalHealHorror
  pure $ a & (horrorHealedL .~ amount)

handleAdditionalHealHorror a@InvestigatorAttrs{..} iid source additional = do
  -- exists to have Callbacks for the total, get from investigatorHorrorHealed
  -- TODO: HERE  MAYBE
  cannotHealHorror <- sourcePerformerHasModifier source CannotHealHorror
  if cannotHealHorror
    then pure $ a & horrorHealedL .~ 0
    else do
      let totalHealed = additional + investigatorHorrorHealed
      when (totalHealed > 0) do
        push $ HealHorror (toTarget iid) source totalHealed
      pure a

handleHealHorror a@InvestigatorAttrs{..} iid source amount' = do
  mods <- getModifiers a
  let n = sum [x | HealingTaken x <- mods]
  let amount = amount' + n
  cannotHealHorror <- sourcePerformerHasModifier source CannotHealHorror
  unless cannotHealHorror
    $ pushAll [HealHorrorDelayed (InvestigatorTarget iid) source amount, ApplyHealing source]
  pure a

handleHealHorrorDelayed a@InvestigatorAttrs{..} target source n msg = do
  cannotHealHorror <- sourcePerformerHasModifier source CannotHealHorror

  -- afterWindow <- checkWindows [mkAfter $ Window.Healed #horror (toTarget a) source n]

  unless cannotHealHorror do
    hrrTreacheries <-
      selectWithField TreacheryCard $ treacheryInThreatAreaOf investigatorId
        <> TreacheryWithModifier IsPointOfHorror
    mods <- getModifiers a
    let onlyTargets = [targetLabel t [HealHorror t source 1] | CannotHealHorrorOnOtherCards t <- mods]
    let additionalTargets =
          guard (null onlyTargets)
            *> [targetLabel t [HealHorror t source 1] | HealHorrorAsIfOnInvestigator t x <- mods, x > 0]

    let remainingHorror =
          length hrrTreacheries
            + investigatorSanityDamage a
            + investigatorAssignedSanityDamage
            - sum (toList investigatorAssignedSanityHeal)
    if null additionalTargets && null onlyTargets
      then do
        let canHealAtFullSources = [sourceMatcher | CanHealAtFull sourceMatcher DamageType <- mods]
        canHealAtFull <-
          if null canHealAtFullSources
            then pure False
            else sourceMatches source (mconcat canHealAtFullSources)
        when (remainingHorror > 0 || canHealAtFull) do
          push $ Do msg
      else do
        player <- getPlayer a.id
        pushAll
          [ chooseOne player
              $ [HorrorLabel a.id [Do $ HealHorrorDelayed target source 1] | remainingHorror > 0, null onlyTargets]
              <> additionalTargets
              <> onlyTargets
          , HealHorrorDelayed target source (n - 1)
          ]

  pure a

handleDoHealHorrorDelayed a@InvestigatorAttrs{..} source n = do
  let iid = investigatorId
  cannotHealHorror <- sourcePerformerHasModifier source CannotHealHorror
  unless cannotHealHorror do
    hrrTreacheries <-
      selectWithField TreacheryCard $ treacheryInThreatAreaOf iid <> TreacheryWithModifier IsPointOfHorror
    if null hrrTreacheries
      then push $ Do (HealHorror (InvestigatorTarget iid) source n)
      else do
        player <- getPlayer iid
        push
          $ chooseOne player
          $ [ Label
                (withI18n $ cardNameVar c $ ikey' "label.healName")
                $ toDiscardBy iid source t
                : [Do (HealHorrorDelayed (InvestigatorTarget iid) source (n - 1)) | n - 1 > 0]
            | (t, c) <- hrrTreacheries
            ]
          <> [ Label "$label.healRemainingHorrorNormally" [Do (HealHorror (toTarget iid) source n)]
             | investigatorSanityDamage a > 0
             ]
  pure a

handleDoHealHorror a@InvestigatorAttrs{..} source n = do
  cannotHealHorror <- sourcePerformerHasModifier source CannotHealHorror
  if cannotHealHorror
    then pure a
    else pure $ a & assignedSanityHealL %~ insertWith (+) source n

handleReassignHorror a@InvestigatorAttrs{..} n = do
  pure $ a & assignedSanityDamageL %~ max 0 . subtract n

handleReassignDamage a@InvestigatorAttrs{..} n = do
  pure $ a & assignedHealthDamageL %~ max 0 . subtract n

handleHealHorrorDirectly a@InvestigatorAttrs{..} iid source amount = do
  -- USE ONLY WHEN NO CALLBACKS
  let totalSanity = amount + investigatorHorrorHealed
  let overHealSanity = max 0 (totalSanity - a.sanityDamage - a.assignedSanityDamage)

  pushWhen (overHealSanity > 0) $ ExcessHealHorror a.id source overHealSanity

  a' <-
    if amount > 0 then removeInvestigatorTokens #horror amount a else pure a
  pure
    $ a'
    & (unhealedHorrorThisRoundL %~ min 0 . subtract amount)

handleHealDamageDirectly a@InvestigatorAttrs{..} iid source amount = do
  -- USE ONLY WHEN NO CALLBACKS
  cannotHealDamage <- sourcePerformerHasModifier source CannotHealDamage
  if cannotHealDamage
    then pure a
    else do
      let overHealDamage = max 0 (amount - a.healthDamage - a.assignedHealthDamage)
      pushWhen (overHealDamage > 0) $ ExcessHealDamage a.id source overHealDamage

      removeInvestigatorTokens #damage amount a

handleInvestigatorWhenDefeated a@InvestigatorAttrs{..} source iid = do
  modifiedHealth <- field InvestigatorHealth (toId a)
  modifiedSanity <- field InvestigatorSanity (toId a)
  let
    defeatedByHorror = investigatorSanityDamage a >= modifiedSanity
    defeatedByDamage = investigatorHealthDamage a >= modifiedHealth
    defeatedBy = case (defeatedByHorror, defeatedByDamage) of
      (True, True) -> DefeatedByDamageAndHorror source
      (True, False) -> DefeatedByHorror source
      (False, True) -> DefeatedByDamage source
      (False, False) -> DefeatedByOther source
  windowMsg <- checkWindows [mkWhen $ Window.InvestigatorDefeated defeatedBy iid]
  pushAll [windowMsg, InvestigatorIsDefeated source iid]
  pure a

handleInvestigatorKilled a@InvestigatorAttrs{..} source iid = do
  unless investigatorDefeated do
    isLead <- (== iid) <$> getLead
    pushAll $ [ChooseLeadInvestigator | isLead] <> [Msg.InvestigatorDefeated source iid]
  pure $ a & defeatedL .~ True & endedTurnL .~ True & killedL .~ True

handleSufferTrauma a@InvestigatorAttrs{..} iid physical mental = do
  push $ CheckTrauma iid
  pure $ a & physicalTraumaL +~ physical & mentalTraumaL +~ mental

handleSetTrauma a@InvestigatorAttrs{..} iid physical mental = do
  pure $ a & physicalTraumaL .~ physical & mentalTraumaL .~ mental

handleCheckTrauma a@InvestigatorAttrs{..} iid = do
  pushWhen (investigatorMentalTrauma >= investigatorSanity) $ DrivenInsane iid
  pushWhen (investigatorPhysicalTrauma >= investigatorHealth) $ InvestigatorKilled (toSource a) iid
  pushWhen
    (investigatorMentalTrauma >= investigatorSanity || investigatorPhysicalTrauma >= investigatorHealth)
    CheckForRemainingInvestigators
  pure a

handleHealTrauma a@InvestigatorAttrs{..} iid physical mental = do
  pure
    $ a
    & (physicalTraumaL %~ max 0 . subtract physical)
    & (mentalTraumaL %~ max 0 . subtract mental)

getHealthDamageableAssets
  :: (HasGame m, Tracing m)
  => InvestigatorId
  -> AssetMatcher
  -> Source
  -> Int
  -> [Target]
  -> [Target]
  -> m (Set AssetId)
getHealthDamageableAssets _ _ _ 0 _ _ = pure mempty
getHealthDamageableAssets iid matcher source _ damageTargets horrorTargets = do
  allAssets <- select $ matcher <> AssetCanBeAssignedDamageBy iid <> AssetCanBeDamagedBySource source
  excludes <- do
    modifiers <- getModifiers iid
    excludeMatchers <- forMaybeM modifiers $ \case
      NoMoreThanOneDamageOrHorrorAmongst excludeMatcher -> do
        excludes <- selectMap AssetTarget excludeMatcher
        pure $ do
          guard $ any (`elem` excludes) (damageTargets <> horrorTargets)
          pure excludeMatcher
      _ -> pure Nothing
    case excludeMatchers of
      [] -> pure mempty
      xs -> select (AssetOneOf xs)
  -- For deferred assignment the tokens aren't placed yet, so AssetRemainingHealth
  -- still reads full. Drop assets already filled by damage assigned earlier in this
  -- same assignment (assigned is 0 for the immediate strategies, so they're unaffected).
  notFull <- flip filterM allAssets \aid -> do
    remaining <- fieldMap AssetRemainingHealth (fromMaybe 0) aid
    assigned <- field AssetAssignedHealthDamage aid
    pure $ remaining - assigned > 0
  pure $ setFromList $ filter (`notElem` excludes) notFull

getSanityDamageableAssets
  :: (HasGame m, Tracing m)
  => InvestigatorId
  -> AssetMatcher
  -> Source
  -> Int
  -> [Target]
  -> [Target]
  -> m (Set AssetId)
getSanityDamageableAssets _ _ _ 0 _ _ = pure mempty
getSanityDamageableAssets iid matcher source _ damageTargets horrorTargets = do
  allAssets <- select $ matcher <> AssetCanBeAssignedHorrorBy iid <> AssetCanBeDamagedBySource source
  excludes <- do
    modifiers <- getModifiers iid
    excludeMatchers <- forMaybeM modifiers $ \case
      NoMoreThanOneDamageOrHorrorAmongst excludeMatcher -> do
        excludes <- selectMap AssetTarget excludeMatcher
        pure $ do
          guard $ any (`elem` excludes) (damageTargets <> horrorTargets)
          pure excludeMatcher
      _ -> pure Nothing
    case excludeMatchers of
      [] -> pure mempty
      xs -> select (AssetOneOf xs)
  -- See getHealthDamageableAssets: drop assets already filled by horror assigned
  -- earlier in this same (deferred) assignment.
  notFull <- flip filterM allAssets \aid -> do
    remaining <- fieldMap AssetRemainingSanity (fromMaybe 0) aid
    assigned <- field AssetAssignedSanityDamage aid
    pure $ remaining - assigned > 0
  pure $ setFromList $ filter (`notElem` excludes) notFull

sourcePerformerHasModifier :: (HasGame m, Tracing m) => Source -> ModifierType -> m Bool
sourcePerformerHasModifier source m =
  getSourceController source >>= \case
    Nothing -> pure False
    Just iid -> hasModifier iid m

getFacingDefeat :: (HasGame m, Tracing m) => InvestigatorAttrs -> m Bool
getFacingDefeat a@InvestigatorAttrs {..} = do
  canOnlyBeDefeatedByDamage <- hasModifier a CanOnlyBeDefeatedByDamage
  modifiedHealth <- field InvestigatorHealth (toId a)
  modifiedSanity <- field InvestigatorSanity (toId a)
  pure
    $ or
      [ investigatorHealthDamage a + investigatorAssignedHealthDamage >= modifiedHealth
      , and
          [ investigatorSanityDamage a + investigatorAssignedSanityDamage >= modifiedSanity
          , not canOnlyBeDefeatedByDamage
          ]
      ]
