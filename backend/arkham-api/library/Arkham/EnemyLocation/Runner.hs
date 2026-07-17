{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.EnemyLocation.Runner (module Arkham.EnemyLocation.Runner, module X) where

import Arkham.Ability as X
import Arkham.Behavior.Damage qualified as Damage
import Arkham.Behavior.Defeat qualified as Defeat
import Arkham.Behavior.Evade qualified as Evade
import Arkham.Behavior.Fight qualified as Fight
import Arkham.Behavior.Heal qualified as Heal
import Arkham.Behavior.Investigate qualified as Investigate
import Arkham.Calculation as X
import Arkham.Classes as X
import Arkham.EnemyLocation.Types as X
import Arkham.GameValue as X
import Arkham.Helpers.Ability as X
import Arkham.Helpers.Message as X (push, pushAll, pushM, runQueueT)
import Arkham.Helpers.Query as X
import Arkham.Id as X
import Arkham.Location.Base as X (LocationAttrs (..))
import Arkham.Source as X
import Arkham.Target as X

import Arkham.Action qualified as Action
import Arkham.Attack (enemyAttack)
import Arkham.Attack.Types (AttackTarget (..), EnemyAttackDetails (..), EnemyAttackType (..))
import Arkham.Capability
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Constants
import Arkham.Damage (DamageType (..))
import Arkham.DamageEffect (DamageAssignment (..))
import Arkham.Direction
import Arkham.Discover (DiscoverLocation (DiscoverAtLocation))
import Arkham.ForMovement (ForMovement (..))
import Arkham.Helpers.Calculation (calculate)
import Arkham.Helpers.Discover (resolveDiscoverCluesAt, resolveSuccessfulInvestigation)
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Source (getSourceController)
import Arkham.Helpers.Window (checkAfter, checkWindows, frame)
import Arkham.History
import Arkham.Investigator.Types (Field (..))
import Arkham.Keyword qualified as Keyword
import Arkham.Location.Base (directionsL, labelL, positionL, tokensL, withoutCluesL)
import Arkham.Location.Grid
import Arkham.Matcher (
  InvestigatorMatcher (UneliminatedInvestigator, You),
  LocationMatcher (..),
  accessibleTo,
  noModifier,
  pattern YourLocation,
 )
import Arkham.Message
import Arkham.Message qualified as Msg
import Arkham.Name (display, toName)
import Arkham.Prelude
import Arkham.Projection
import Arkham.Token
import Arkham.Tracing
import Arkham.Trait
import Arkham.Window qualified as Window
import Data.Map.Strict qualified as Map

-- | The coerced EnemyId used for fight/evade targeting by the enemy subsystem.
asEnemyId :: EnemyLocationAttrs -> EnemyId
asEnemyId = enemyLocationAsEnemyId . EnemyLocationId . toId

isEnemyTarget :: EnemyLocationAttrs -> Target -> Bool
isEnemyTarget a target =
  isTarget (EnemyId $ coerce $ unLocationId a.id) target || isTarget a target

instance HasModifiersFor EnemyLocationAttrs where
  getModifiersFor a = do
    -- All enemy-locations: cannot make attacks of opportunity, cannot be moved
    -- by card effects (rules: "Enemy-locations cannot be moved by card effects").
    modifySelf a [CannotMakeAttacksOfOpportunity]

instance HasAbilities EnemyLocationAttrs where
  getAbilities a =
    [ basicAbility
        $ restricted a AbilityAttack (OnSameLocation <> CanAttack)
        $ ActionAbility #fight #combat (ActionCost 1)
    , basicAbility
        $ restricted a AbilityEvade OnSameLocation
        $ ActionAbility #evade #agility (ActionCost 1)
    , basicAbility
        $ investigateAbility
          a
          AbilityInvestigate
          mempty
          (OnSameLocation <> exists (YourLocation <> InvestigatableLocation))
    , basicAbility
        $ restricted
          a
          AbilityMove
          ( CanMoveTo (LocationWithId a.id)
              <> OnLocation (IncludeEmptySpace $ accessibleTo ForMovement a)
              <> exists (You <> can.move <> noModifier (CannotEnter a.id))
          )
        $ ActionAbility #move Nothing (ActionCost 1)
    ]

instance RunMessage EnemyLocationAttrs where
  runMessage msg a = runQueueT $ case msg of
    UseAbility _ ab _ | isSource a ab.source || isProxySource a ab.source -> do
      push $ Do msg
      pure a
    UseCardAbility iid (isSource a -> True) AbilityAttack _ _ -> do
      Fight.pushAttackAbility (asEnemyId a) iid (a.ability AbilityAttack)
      pure a
    -- Fight system routes AttackEnemy via coerced EnemyId.
    AttackEnemy eid choose | eid == asEnemyId a -> do
      Fight.resolveAttack (asEnemyId a) a.fight choose
      pure a
    UseCardAbility iid (isSource a -> True) AbilityEvade _ _ -> do
      Evade.pushEvadeAbility (asEnemyId a) iid (a.ability AbilityEvade)
      pure a
    UseCardAbility iid (isSource a -> True) AbilityMove _ _ -> do
      push $ MoveAction iid a.id Free False
      pure a
    -- Evade system routes TryEvadeEnemy via coerced EnemyId.
    TryEvadeEnemy sid iid eid source mTarget skillType | eid == asEnemyId a -> do
      Evade.resolveTryEvade (asEnemyId a) a.evade sid iid source mTarget skillType
      pure a
    UseCardAbility iid (isSource a -> True) AbilityInvestigate _ _ -> do
      Investigate.pushInvestigateAbility a.id iid (a.ability AbilityInvestigate)
      pure a
    Investigate investigation | investigation.location == a.id && not investigation.isAction -> do
      allowed <- getInvestigateAllowed investigation.investigator a
      when allowed $ Investigate.resolveInvestigateAtShroud a a.shroud investigation
      pure a
    PassedSkillTest iid (Just Action.Investigate) source (Initiator target) _ n | isTarget a target -> do
      let clues = a.clues
      let (before, _, after) = frame $ Window.SuccessfullyInvestigateWithNoClues iid $ toId a
      push
        $ SkillTestResultOption
        $ SkillTestOption
          { option =
              Label ("Discover Clue at " <> display (toName a))
                $ [before | clues == 0]
                <> [ UpdateHistory iid (HistoryItem HistorySuccessfulInvestigations 1)
                   , Successful (Action.Investigate, toTarget a) iid source (toTarget a) n
                   ]
                <> [after | clues == 0]
          , kind = OriginalOptionKind
          , criteria = Nothing
          }
      pure a
    -- Enemy-locations aren't regular Location entities, so the Location runner's
    -- Successful/DiscoverClues handlers never fire for them. Mirror them here via
    -- the shared helpers so investigators can actually discover clues on an
    -- enemy-location (e.g. a Living Bedroom holding clues).
    Successful (Action.Investigate, _) iid source target n | isTarget a target -> do
      resolveSuccessfulInvestigation a.id (toSource a) iid source n
      pure a
    Msg.DiscoverClues iid d | d.location == DiscoverAtLocation a.id -> do
      resolveDiscoverCluesAt a.id iid d
      pure a
    PassedSkillTest iid (Just Action.Fight) source (Initiator target) _ n | isEnemyTarget a target -> do
      Fight.pushSuccessfulAttack iid source (asEnemyId a) n
      pure a
    PassedSkillTest iid (Just Action.Evade) source (Initiator target) _ n | isEnemyTarget a target -> do
      Evade.pushSuccessfulEvade iid source (asEnemyId a) n
      pure a
    FailedSkillTest iid (Just Action.Fight) _ (Initiator target) _ _ | isEnemyTarget a target -> do
      mods <- getCombinedModifiers [toTarget iid, toTarget a]
      let keywords = cdKeywords (toCardDef a)
      when
        ( Keyword.Retaliate
            `member` keywords
            && IgnoreRetaliate
            `notElem` mods
            && (not a.exhausted || CanRetaliateWhileExhausted `elem` mods)
        )
        $ push
        $ EnemyAttack
        $ (enemyAttack (asEnemyId a) a iid) {attackType = RetaliateAttack}
      pure a
    EnemyEvaded _ eid | eid == asEnemyId a -> pure $ a & exhaustedL .~ True
    Exhaust ea | isEnemyTarget a ea.target -> pure $ a & exhaustedL .~ True
    ReadyExhausted | not a.defeated -> do
      when a.exhausted $ push $ Ready (toTarget a)
      pure a
    Ready target | isEnemyTarget a target -> pure $ a & exhaustedL .~ False
    -- Push EnemyAttack directly rather than EnemyWillAttack: EnemyWillAttack routes
    -- through chooseOneAtATime with an EnemyTarget label the frontend can't resolve
    -- for a coerced EnemyId that doesn't exist in enemiesL.
    -- Use field InvestigatorLocation rather than InvestigatorAt: enemy-locations live
    -- in enemyLocationsL, not locationsL, so InvestigatorAt always returns empty.
    Do EnemiesAttack | not a.exhausted && not a.defeated -> do
      let eid = asEnemyId a
      iids <-
        filterM (\iid -> (== Just a.id) <$> field InvestigatorLocation iid)
          =<< select UneliminatedInvestigator
      unless (null iids) do
        let details = case iids of
              [x] -> enemyAttack eid a x
              (x : _) ->
                (enemyAttack eid a x)
                  { attackTarget = MassiveAttackTargets (map toTarget iids)
                  , attackOriginalTarget = MassiveAttackTargets (map toTarget iids)
                  }
              [] -> error "unreachable"
        push $ EnemyAttack details
      pure a
    -- EnemyAttack for a coerced EnemyId has no real enemy entity to route to; handle here.
    EnemyAttack details | details.enemy == asEnemyId a -> do
      push $ Do (EnemyAttack details)
      pure a
    -- PerformEnemyAttack reads EnemyHealthDamage/EnemySanityDamage fields from a real
    -- enemy entity — apply damage directly from attrs instead.
    Do (EnemyAttack details) | attackEnemy details == asEnemyId a -> do
      let eid = asEnemyId a
      let hDmg = if attackDealDamage details then a.healthDamage else 0
      let sDmg = a.sanityDamage
      whenWindow <- checkWindows [Window.mkWhen $ Window.EnemyAttacks details]
      afterAttacksWindow <- checkWindows [Window.mkAfter $ Window.EnemyAttacks details]
      afterWindow <- checkWindows [Window.mkAfter $ Window.EnemyAttacksEvenIfCancelled details]
      let dmgMsg iid' =
            InvestigatorAssignDamage iid' (EnemyAttackSource eid) (attackDamageStrategy details) hDmg sDmg
      let targets = case attackTarget details of
            SingleAttackTarget (InvestigatorTarget iid') -> [iid']
            MassiveAttackTargets ts -> [iid' | InvestigatorTarget iid' <- ts]
            _ -> []
      pushAll
        $ [whenWindow]
        <> [dmgMsg iid' | not details.cancelled, iid' <- targets]
        <> [afterAttacksWindow]
        <> [afterWindow]
      pure a
    InitiateEnemyAttack details | details.enemy == asEnemyId a -> do
      unless a.exhausted $ push $ EnemyAttack details
      pure a
    DealDamage (EnemyTarget eid) da | eid == asEnemyId a && not a.defeated -> do
      let amount = da.amount
      let source = da.source
      let damageEffect = da.effect
      modifiers' <- getModifiers (LocationTarget a.id)
      let modifiedAmount = foldr applyDamageMod amount modifiers'
      when (modifiedAmount > 0) do
        Damage.fireDamageWindows source (toTarget (asEnemyId a)) damageEffect modifiedAmount do
          push $ Msg.Damaged (EnemyTarget eid) da {damageAssignmentAmount = modifiedAmount}
        push $ CheckDefeated source (toTarget a)
      pure $ a & baseL . tokensL %~ addTokens Damage modifiedAmount
    HealDamage (EnemyTarget eid) source n | eid == asEnemyId a -> do
      let healAmount = min n (enemyLocationDamage a)
      when (healAmount > 0) do
        Heal.pushHealedAfter DamageType (toTarget a) source healAmount
      pure $ a & baseL . tokensL %~ subtractTokens Damage healAmount
    CheckDefeated source (isTarget a -> True) | not a.defeated -> do
      mModifiedHealth <- getModifiedHealth a
      for_ mModifiedHealth \health -> do
        when (enemyLocationDamage a >= health) do
          (whenMsg, afterMsg) <- Defeat.wouldBeDefeatedWindows (asEnemyId a)
          pushAll
            [ whenMsg
            , afterMsg
            , Msg.Defeated (EnemyTarget (asEnemyId a)) (toCardId a) source (setToList $ toTraits (toCardDef a))
            ]
      pure a
    -- Enemy-locations are enemies, so they defeat through the same generic `Defeated`
    -- message and the same Defeat behavior as Arkham.Enemy.Runner: that is what lets
    -- "if this attack defeats an enemy" effects (Meat Cleaver, Hatchet, Runic Axe, ...),
    -- the Game runner's defeat history, and cancelEnemyDefeat see them. The enemy
    -- subsystem has no entity for the coerced EnemyId, so resolve the trio here.
    Msg.Defeated (EnemyTarget eid) _ source _ | eid == asEnemyId a -> do
      defeatedBy <- Defeat.classifyDefeat source (enemyLocationDamage a) <$> getModifiedHealth a
      miid <- getSourceController source
      pushAll =<< Defeat.openDefeat (asEnemyId a) defeatedBy miid msg []
      pure a
    Do (Msg.Defeated (EnemyTarget eid) _ source _) | eid == asEnemyId a -> do
      defeatedBy <- Defeat.classifyDefeat source (enemyLocationDamage a) <$> getModifiedHealth a
      miid <- getSourceController source
      -- No disposal: an enemy-location doesn't discard itself. Removing it also removes
      -- the location and relocates everyone standing on it, which is a consequence of
      -- the defeat rather than part of it, so that waits for `After` below — otherwise
      -- IfEnemyDefeated would resolve against a location that no longer exists and
      -- matchers like Bounty's `EnemyWasAt YourLocation` could never match.
      pushAll =<< Defeat.closeDefeat (asEnemyId a) defeatedBy miid []
      pure a
    After (Msg.Defeated (EnemyTarget eid) _ source _) | eid == asEnemyId a -> do
      push $ Msg.EnemyLocationDefeated a.id (toCardId a) source (setToList $ toTraits (toCardDef a))
      pure $ a & defeatedL .~ True
    -- Hand off to the scenario, which relocates what was here and removes the location
    -- per the enemy-location defeat rules.
    Msg.EnemyLocationDefeated lid _ _ _ | lid == a.id -> do
      push $ ScenarioSpecific "enemyLocationDefeated" (toJSON lid)
      pure a
    PlaceTokens _ (isTarget a -> True) token n -> pure $ a & baseL . tokensL %~ addTokens token n
    RemoveTokens _ (isTarget a -> True) Clue n -> do
      let clueCount = max 0 $ a.clues - n
      when (clueCount == 0 && a.clues > 0) do
        pushM $ checkAfter $ Window.LastClueRemovedFromLocation a.id
      pure $ a & baseL . tokensL %~ setTokens Clue clueCount & baseL . withoutCluesL .~ (clueCount == 0)
    RemoveTokens _ (isTarget a -> True) token n -> pure $ a & baseL . tokensL %~ subtractTokens token n
    -- Mirror the Location runner: MoveTokens splits into a remove from the source
    -- location and a place onto the target. Enemy-locations aren't Location
    -- entities, so without this the "remove from source" half never runs and
    -- discovered clues are duplicated onto the investigator instead of moved.
    MoveTokens s source _ tType n | isSource a source -> liftRunMessage (RemoveTokens s (toTarget a) tType n) a
    MoveTokens _s (InvestigatorSource _) target Clue _ | isTarget a target -> pure a
    MoveTokens s _ target tType n | isTarget a target -> liftRunMessage (PlaceTokens s target tType n) a
    -- Enemy-locations are fixed in the grid and cannot be moved by card effects.
    EnemyMove eid _ | eid == asEnemyId a -> pure a
    SetLocationLabel lid label' | lid == a.id -> pure $ a & baseL . labelL .~ label'
    LocationMoved lid | lid == a.id -> pure $ a & baseL . directionsL .~ mempty
    PlacedLocationDirection lid direction lid2
      | lid2 == a.id ->
          pure $ a & baseL . directionsL %~ Map.insertWith (<>) direction [lid]
    PlacedLocationDirection lid direction lid2
      | lid == a.id ->
          let reversedDirection = case direction of
                LeftOf -> RightOf
                RightOf -> LeftOf
                Above -> Below
                Below -> Above
           in pure $ a & baseL . directionsL %~ Map.insertWith (<>) reversedDirection [lid2]
    PlaceGrid (GridLocation pos lid) | lid == a.id -> pure $ a & baseL . positionL ?~ pos
    SendMessage (isTarget a -> True) msg' -> liftRunMessage msg' a
    _ -> pure a
   where
    applyDamageMod (DamageDealt n) acc = acc + n
    applyDamageMod (DamageTaken n) acc = acc + n
    applyDamageMod NoDamageDealt _ = 0
    applyDamageMod _ acc = acc

{- | This enemy-location's health with 'HealthModifier's applied.

Not @field EnemyHealth@: an enemy-location registers its modifiers against its
'LocationTarget' (see 'modifySelf' in e.g. Living Parlor), so the enemy field
projection over the coerced EnemyId would miss them entirely.
-}
getModifiedHealth :: (Tracing m, HasGame m) => EnemyLocationAttrs -> m (Maybe Int)
getModifiedHealth a = do
  mHealth <- traverse calculate a.health
  modifiers' <- getModifiers (toTarget a)
  let applyHealthMod (HealthModifier m) n = max 0 (n + m)
      applyHealthMod _ n = n
  pure $ fmap (\h -> foldr applyHealthMod h modifiers') mHealth

-- | Determine if an investigator is allowed to investigate this enemy-location.
getInvestigateAllowed :: HasGame m => InvestigatorId -> EnemyLocationAttrs -> m Bool
getInvestigateAllowed _iid attrs = do
  modifiers' <- getModifiers (LocationTarget attrs.id)
  let cannotInvestigate = flip any modifiers' \case
        CannotInvestigateLocation lid -> lid == attrs.id
        CannotInvestigate -> True
        _ -> False
  pure $ not cannotInvestigate && isJust (locationShroud (enemyLocationBase attrs))
