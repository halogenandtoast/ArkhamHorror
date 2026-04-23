{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.EnemyLocation.Runner (module Arkham.EnemyLocation.Runner, module X) where

import Arkham.Ability as X
import Arkham.Calculation as X
import Arkham.Classes as X
import Arkham.EnemyLocation.Types as X
import Arkham.GameValue as X
import Arkham.Helpers.Ability as X
import Arkham.Helpers.Effect as X
import Arkham.Helpers.Message as X hiding (
  EnemyDamage,
  EnemyDefeated,
  InvestigatorDefeated,
  PaidCost,
 )
import Arkham.Helpers.Query as X
import Arkham.Helpers.SkillTest as X
import Arkham.Id as X
import Arkham.Source as X
import Arkham.Target as X

import Arkham.Action qualified as Action
import Arkham.Attack (enemyAttack)
import Arkham.Attack.Types (AttackTarget (..), EnemyAttackDetails (..))
import Arkham.Capability
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Constants
import Arkham.DamageEffect (DamageAssignment (..))
import Arkham.DefeatedBy
import Arkham.Direction
import Arkham.Fight
import Arkham.ForMovement (ForMovement (..))
import Arkham.Helpers.GameValue (getGameValue)
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Source (getSourceController)
import Arkham.Helpers.Window (checkAfter, checkWhen, checkWindows, frame)
import Arkham.History
import Arkham.Investigate
import Arkham.Investigator.Types (Field (..))
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
  getModifiersFor _ = pure ()

instance HasAbilities EnemyLocationAttrs where
  getAbilities a =
    [ -- Fight ability: investigators may fight enemy-locations
      basicAbility
        $ restricted a AbilityAttack (OnSameLocation <> CanAttack)
        $ ActionAbility #fight #combat (ActionCost 1)
    , -- Evade ability: investigators may evade enemy-locations
      basicAbility
        $ restricted a AbilityEvade OnSameLocation
        $ ActionAbility #evade #agility (ActionCost 1)
    , -- Investigate ability: investigators may investigate enemy-locations.
      basicAbility
        $ investigateAbility
          a
          AbilityInvestigate
          mempty
          (OnSameLocation <> exists (YourLocation <> InvestigatableLocation))
    , -- Move ability: investigators may move to enemy-locations from adjacent locations.
      -- Index 102 matches EmptySpace convention; frontend requires this exact index for click-to-move.
      basicAbility
        $ restricted
          a
          102
          ( CanMoveTo (LocationWithId a.id)
              <> OnLocation (IncludeEmptySpace $ accessibleTo ForMovement a)
              <> exists (You <> can.move <> noModifier (CannotEnter a.id))
          )
        $ ActionAbility #move Nothing (ActionCost 1)
    ]

instance RunMessage EnemyLocationAttrs where
  runMessage msg a@EnemyLocationAttrs {..} = runQueueT $ case msg of
    -- Forward UseAbility to the active-investigator path (same as Location/Runner.hs).
    UseAbility _ ab _ | isSource a ab.source || isProxySource a ab.source -> do
      push $ Do msg
      pure a
    -- Fight: investigator uses the fight ability on this enemy-location.
    -- Pushes FightEnemy with the coerced EnemyId so the fight system handles it.
    UseCardAbility iid (isSource a -> True) AbilityAttack _ _ -> do
      sid <- getRandom
      push $ FightEnemy (asEnemyId a) $ mkChooseFightPure sid iid (a.ability AbilityAttack)
      pure a
    -- The fight system routes AttackEnemy to us via our coerced EnemyId.
    -- Resolve the fight skill test against our fight value.
    AttackEnemy eid choose | eid == asEnemyId a -> do
      let iid = choose.investigator
      let source = choose.source
      let sid = choose.skillTest
      let target = maybe (toTarget a) (ProxyTarget (toTarget a)) choose.target
      let skillType = choose.skillType
      let difficulty = case choose.difficulty of
            DefaultChooseFightDifficulty -> fromMaybe (Fixed 0) enemyLocationFight
            CalculatedChooseFightDifficulty ccfd -> ccfd
      push $ fight sid iid source target skillType difficulty
      pure a
    -- Evade: investigator uses the evade ability.
    UseCardAbility iid (isSource a -> True) AbilityEvade _ _ -> do
      sid <- getRandom
      push $ EvadeEnemy sid iid (asEnemyId a) (a.ability AbilityEvade) Nothing #agility False
      pure a
    -- Move: investigator moves to this enemy-location.
    UseCardAbility iid (isSource a -> True) 102 _ _ -> do
      push $ MoveAction iid enemyLocationId Free False
      pure a
    -- The evade system routes TryEvadeEnemy to us via our coerced EnemyId.
    TryEvadeEnemy sid iid eid source mTarget skillType | eid == asEnemyId a -> do
      let target = maybe (toTarget (asEnemyId a)) (ProxyTarget (toTarget (asEnemyId a))) mTarget
      let difficulty = fromMaybe (Fixed 0) enemyLocationEvade
      push $ evade sid iid source target skillType difficulty
      pure a
    -- Investigate: investigator uses the investigate ability.
    UseCardAbility iid (isSource a -> True) AbilityInvestigate _ _ -> do
      let triggerSource = a.ability AbilityInvestigate
      sid <- getRandom
      pushM $ mkInvestigateLocation sid iid triggerSource enemyLocationId
      pure a
    -- Investigate message from the Investigate system targeting this location.
    Investigate investigation | investigation.location == enemyLocationId && not investigation.isAction -> do
      let iid = investigation.investigator
      allowed <- getInvestigateAllowed iid a
      when allowed $ do
        let target = maybe (toTarget a) (ProxyTarget (toTarget a)) investigation.target
        push
          $ investigate investigation.skillTest iid investigation.source target investigation.skillType
          $ maybe (Fixed 0) GameValueCalculation enemyLocationShroud
      pure a
    -- Successful investigation: discover clues at this enemy-location.
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
    -- Successful fight: apply damage to the enemy-location.
    PassedSkillTest iid (Just Action.Fight) source (Initiator target) _ n | isEnemyTarget a target -> do
      let eid = asEnemyId a
      whenMsg <- checkWhen $ Window.SuccessfulAttackEnemy iid source eid n
      afterMsg <- checkAfter $ Window.SuccessfulAttackEnemy iid source eid n
      pushAll
        [ whenMsg
        , InvestigatorDamageEnemy iid eid source
        , Successful (Action.Fight, toTarget eid) iid source (toTarget eid) n
        , afterMsg
        ]
      pure a
    -- Successful evade: push EnemyEvaded (which will exhaust).
    PassedSkillTest iid (Just Action.Evade) source (Initiator target) _ n | isEnemyTarget a target -> do
      let eid = asEnemyId a
      whenMsg <- checkWhen $ Window.SuccessfulEvadeEnemy iid source eid n
      afterMsg <- checkAfter $ Window.SuccessfulEvadeEnemy iid source eid n
      pushAll [whenMsg, EnemyEvaded iid eid, afterMsg]
      pure a
    -- Evaded: exhaust this enemy-location.
    EnemyEvaded _ eid
      | eid == asEnemyId a ->
          pure $ a & exhaustedL .~ True
    -- Upkeep: ready this enemy-location if it is exhausted.
    ReadyExhausted | not enemyLocationDefeated -> do
      when enemyLocationExhausted $ push $ Ready (toTarget a)
      pure a
    -- Ready: unexhaust this enemy-location.
    Ready target
      | isEnemyTarget a target ->
          pure $ a & exhaustedL .~ False
    -- Enemy phase: attack all investigators at this location.
    -- We push EnemyAttack directly (not EnemyWillAttack) because EnemyWillAttack
    -- routes through Game/Runner.hs which wraps attacks in chooseOneAtATime with
    -- an EnemyTarget label — the frontend can't resolve the coerced EnemyId for
    -- a location entity, so the choice would never be rendered or auto-resolved.
    Do EnemiesAttack | not enemyLocationExhausted && not enemyLocationDefeated -> do
      let eid = asEnemyId a
      iids <-
        filterM (\iid -> (== Just enemyLocationId) <$> field InvestigatorLocation iid)
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
    -- EnemyAttack for a coerced EnemyId won't match any real enemy; handle here.
    EnemyAttack details | details.enemy == asEnemyId a -> do
      push $ Do (EnemyAttack details)
      pure a
    -- Apply damage directly (PerformEnemyAttack relies on real enemy entity fields).
    Do (EnemyAttack details) | attackEnemy details == asEnemyId a -> do
      let eid = asEnemyId a
      let hDmg = if attackDealDamage details then enemyLocationHealthDamage else 0
      let sDmg = enemyLocationSanityDamage
      whenWindow <- checkWindows [Window.mkWhen $ Window.EnemyAttacks details]
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
        <> [afterWindow]
      pure a
    -- Attack via card effect: skip if exhausted.
    InitiateEnemyAttack details | details.enemy == asEnemyId a -> do
      unless enemyLocationExhausted $ push $ EnemyAttack details
      pure a
    -- Damage applied to this enemy-location (via coerced EnemyId).
    EnemyDamage eid da | eid == asEnemyId a && not enemyLocationDefeated -> do
      let amount = da.amount
      let source = da.source
      modifiers' <- getModifiers (LocationTarget enemyLocationId)
      let modifiedAmount = foldr applyDamageMod amount modifiers'
      when (modifiedAmount > 0) do
        push $ Msg.EnemyDamaged eid da {damageAssignmentAmount = modifiedAmount}
        push $ CheckDefeated source (toTarget a)
      pure $ a & tokensL %~ addTokens Damage modifiedAmount
    -- Check if the enemy-location has been defeated.
    CheckDefeated source (isTarget a -> True) | not enemyLocationDefeated -> do
      mHealth <- traverse (getGameValue . unGameCalculation) enemyLocationHealth
      for_ mHealth \health -> do
        when (enemyLocationDamage a >= health) do
          whenMsg <- checkWindows [Window.mkWhen $ Window.EnemyWouldBeDefeated (asEnemyId a)]
          afterMsg <- checkWindows [Window.mkAfter $ Window.EnemyWouldBeDefeated (asEnemyId a)]
          pushAll
            [ whenMsg
            , afterMsg
            , Msg.EnemyLocationDefeated enemyLocationId (toCardId a) source (setToList $ toTraits (toCardDef a))
            ]
      pure a
    -- The enemy-location has been defeated. Add to victory display and remove.
    Msg.EnemyLocationDefeated lid _ source _ | lid == enemyLocationId -> do
      miid <- getSourceController source
      let defeatedBy = DefeatedByOther source
      whenMsg <- checkWindows [Window.mkWhen $ Window.EnemyDefeated miid defeatedBy (asEnemyId a)]
      afterMsg <- checkWindows [Window.mkAfter $ Window.EnemyDefeated miid defeatedBy (asEnemyId a)]
      pushAll
        [ whenMsg
        , Do (Msg.EnemyLocationDefeated lid (toCardId a) source (setToList $ toTraits (toCardDef a)))
        , afterMsg
        ]
      pure $ a & defeatedL .~ True
    Do (Msg.EnemyLocationDefeated lid _ _ _) | lid == enemyLocationId -> do
      push $ ScenarioSpecific "enemyLocationDefeated" (toJSON lid)
      pure a
    -- Add/remove tokens on the enemy-location as a location target.
    PlaceTokens _ (isTarget a -> True) token n -> do
      pure $ a & tokensL %~ addTokens token n
    RemoveTokens _ (isTarget a -> True) token n -> do
      pure $ a & tokensL %~ subtractTokens token n
    -- EnemyLocation cannot be moved by card effects; ignore EnemyMove messages.
    EnemyMove eid _ | eid == asEnemyId a -> pure a
    -- Sync label when the grid slides this enemy-location.
    SetLocationLabel lid label'
      | lid == enemyLocationId ->
          pure $ a & labelL .~ label'
    -- Clear directions when this enemy-location is repositioned in the grid.
    LocationMoved lid
      | lid == enemyLocationId ->
          pure $ a & directionsL .~ mempty
    -- Update direction map when this enemy-location gains a neighbour.
    PlacedLocationDirection lid direction lid2
      | lid2 == enemyLocationId ->
          pure $ a & directionsL %~ Map.insertWith (<>) direction [lid]
    PlacedLocationDirection lid direction lid2
      | lid == enemyLocationId ->
          let reversedDirection = case direction of
                LeftOf -> RightOf
                RightOf -> LeftOf
                Above -> Below
                Below -> Above
           in pure $ a & directionsL %~ Map.insertWith (<>) reversedDirection [lid2]
    -- Track position updates (for grid-based scenarios).
    PlaceGrid (GridLocation pos lid) | lid == enemyLocationId -> do
      pure $ a & positionL ?~ pos
    -- Meta update
    _ -> pure a
   where
    applyDamageMod (DamageDealt n) acc = acc + n
    applyDamageMod (DamageTaken n) acc = acc + n
    applyDamageMod NoDamageDealt _ = 0
    applyDamageMod _ acc = acc

    unGameCalculation :: GameCalculation -> GameValue
    unGameCalculation (GameValueCalculation gv) = gv
    unGameCalculation (Fixed n) = Static n
    unGameCalculation (SumCalculation _) = Static 0 -- simplified
    unGameCalculation _ = Static 0

-- | Determine if an investigator is allowed to investigate this enemy-location.
getInvestigateAllowed :: HasGame m => InvestigatorId -> EnemyLocationAttrs -> m Bool
getInvestigateAllowed _iid attrs = do
  modifiers' <- getModifiers (LocationTarget attrs.id)
  let cannotInvestigate = flip any modifiers' \case
        CannotInvestigateLocation lid -> lid == attrs.id
        CannotInvestigate -> True
        _ -> False
  pure $ not cannotInvestigate
