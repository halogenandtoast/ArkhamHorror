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
  runMessage msg a@EnemyLocationAttrs {..} = runQueueT $ case msg of
    UseAbility _ ab _ | isSource a ab.source || isProxySource a ab.source -> do
      push $ Do msg
      pure a
    UseCardAbility iid (isSource a -> True) AbilityAttack _ _ -> do
      sid <- getRandom
      push $ FightEnemy (asEnemyId a) $ mkChooseFightPure sid iid (a.ability AbilityAttack)
      pure a
    -- Fight system routes AttackEnemy via coerced EnemyId.
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
    UseCardAbility iid (isSource a -> True) AbilityEvade _ _ -> do
      sid <- getRandom
      push $ EvadeEnemy sid iid (asEnemyId a) (a.ability AbilityEvade) Nothing #agility False
      pure a
    UseCardAbility iid (isSource a -> True) AbilityMove _ _ -> do
      push $ MoveAction iid enemyLocationId Free False
      pure a
    -- Evade system routes TryEvadeEnemy via coerced EnemyId.
    TryEvadeEnemy sid iid eid source mTarget skillType | eid == asEnemyId a -> do
      let target = maybe (toTarget (asEnemyId a)) (ProxyTarget (toTarget (asEnemyId a))) mTarget
      let difficulty = fromMaybe (Fixed 0) enemyLocationEvade
      push $ evade sid iid source target skillType difficulty
      pure a
    UseCardAbility iid (isSource a -> True) AbilityInvestigate _ _ -> do
      let triggerSource = a.ability AbilityInvestigate
      sid <- getRandom
      pushM $ mkInvestigateLocation sid iid triggerSource enemyLocationId
      pure a
    Investigate investigation | investigation.location == enemyLocationId && not investigation.isAction -> do
      let iid = investigation.investigator
      allowed <- getInvestigateAllowed iid a
      when allowed $ do
        let target = maybe (toTarget a) (ProxyTarget (toTarget a)) investigation.target
        push
          $ investigate investigation.skillTest iid investigation.source target investigation.skillType
          $ maybe (Fixed 0) GameValueCalculation enemyLocationShroud
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
    PassedSkillTest iid (Just Action.Evade) source (Initiator target) _ n | isEnemyTarget a target -> do
      let eid = asEnemyId a
      whenMsg <- checkWhen $ Window.SuccessfulEvadeEnemy iid source eid n
      afterMsg <- checkAfter $ Window.SuccessfulEvadeEnemy iid source eid n
      pushAll [whenMsg, EnemyEvaded iid eid, afterMsg]
      pure a
    EnemyEvaded _ eid | eid == asEnemyId a -> pure $ a & exhaustedL .~ True
    ReadyExhausted | not enemyLocationDefeated -> do
      when enemyLocationExhausted $ push $ Ready (toTarget a)
      pure a
    Ready target | isEnemyTarget a target -> pure $ a & exhaustedL .~ False
    -- Push EnemyAttack directly rather than EnemyWillAttack: EnemyWillAttack routes
    -- through chooseOneAtATime with an EnemyTarget label the frontend can't resolve
    -- for a coerced EnemyId that doesn't exist in enemiesL.
    -- Use field InvestigatorLocation rather than InvestigatorAt: enemy-locations live
    -- in enemyLocationsL, not locationsL, so InvestigatorAt always returns empty.
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
    -- EnemyAttack for a coerced EnemyId has no real enemy entity to route to; handle here.
    EnemyAttack details | details.enemy == asEnemyId a -> do
      push $ Do (EnemyAttack details)
      pure a
    -- PerformEnemyAttack reads EnemyHealthDamage/EnemySanityDamage fields from a real
    -- enemy entity — apply damage directly from attrs instead.
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
    InitiateEnemyAttack details | details.enemy == asEnemyId a -> do
      unless enemyLocationExhausted $ push $ EnemyAttack details
      pure a
    EnemyDamage eid da | eid == asEnemyId a && not enemyLocationDefeated -> do
      let amount = da.amount
      let source = da.source
      modifiers' <- getModifiers (LocationTarget enemyLocationId)
      let modifiedAmount = foldr applyDamageMod amount modifiers'
      when (modifiedAmount > 0) do
        push $ Msg.EnemyDamaged eid da {damageAssignmentAmount = modifiedAmount}
        push $ CheckDefeated source (toTarget a)
      pure $ a & tokensL %~ addTokens Damage modifiedAmount
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
    PlaceTokens _ (isTarget a -> True) token n -> pure $ a & tokensL %~ addTokens token n
    RemoveTokens _ (isTarget a -> True) token n -> pure $ a & tokensL %~ subtractTokens token n
    -- Enemy-locations are fixed in the grid and cannot be moved by card effects.
    EnemyMove eid _ | eid == asEnemyId a -> pure a
    SetLocationLabel lid label' | lid == enemyLocationId -> pure $ a & labelL .~ label'
    LocationMoved lid | lid == enemyLocationId -> pure $ a & directionsL .~ mempty
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
    PlaceGrid (GridLocation pos lid) | lid == enemyLocationId -> pure $ a & positionL ?~ pos
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
