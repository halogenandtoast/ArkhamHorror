{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.EnemyLocation.Runner (module Arkham.EnemyLocation.Runner, module X) where

import Arkham.Ability as X
import Arkham.Behavior.Damage qualified as Damage
import Arkham.Behavior.Defeat qualified as Defeat
import Arkham.Behavior.Evade qualified as Evade
import Arkham.Behavior.Fight qualified as Fight
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
import Arkham.Attack.Types (AttackTarget (..), EnemyAttackDetails (..))
import Arkham.Capability
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Constants
import Arkham.DamageEffect (DamageAssignment (..))
import Arkham.DefeatedBy
import Arkham.Direction
import Arkham.ForMovement (ForMovement (..))
import Arkham.Helpers.Calculation (calculate)
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Source (getSourceController)
import Arkham.Helpers.Window (checkAfter, checkWindows, frame)
import Arkham.History
import Arkham.Investigator.Types (Field (..))
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
    PassedSkillTest iid (Just Action.Fight) source (Initiator target) _ n | isEnemyTarget a target -> do
      Fight.pushSuccessfulAttack iid source (asEnemyId a) n
      pure a
    PassedSkillTest iid (Just Action.Evade) source (Initiator target) _ n | isEnemyTarget a target -> do
      Evade.pushSuccessfulEvade iid source (asEnemyId a) n
      pure a
    EnemyEvaded _ eid | eid == asEnemyId a -> pure $ a & exhaustedL .~ True
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
    CheckDefeated source (isTarget a -> True) | not a.defeated -> do
      mHealth <- traverse calculate a.health
      for_ mHealth \health -> do
        when (enemyLocationDamage a >= health) do
          (whenMsg, afterMsg) <- Defeat.wouldBeDefeatedWindows (asEnemyId a)
          pushAll
            [ whenMsg
            , afterMsg
            , Msg.EnemyLocationDefeated a.id (toCardId a) source (setToList $ toTraits (toCardDef a))
            ]
      pure a
    Msg.EnemyLocationDefeated lid _ source _ | lid == a.id -> do
      miid <- getSourceController source
      let defeatedBy = DefeatedByOther source
      (whenMsg, afterMsg) <- Defeat.defeatedWindows miid defeatedBy (asEnemyId a)
      pushAll
        [ whenMsg
        , Do (Msg.EnemyLocationDefeated lid (toCardId a) source (setToList $ toTraits (toCardDef a)))
        , afterMsg
        ]
      pure $ a & defeatedL .~ True
    Do (Msg.EnemyLocationDefeated lid _ _ _) | lid == a.id -> do
      push $ ScenarioSpecific "enemyLocationDefeated" (toJSON lid)
      pure a
    PlaceTokens _ (isTarget a -> True) token n -> pure $ a & baseL . tokensL %~ addTokens token n
    RemoveTokens _ (isTarget a -> True) Clue n -> do
      let clueCount = max 0 $ a.clues - n
      when (clueCount == 0 && a.clues > 0) do
        pushM $ checkAfter $ Window.LastClueRemovedFromLocation a.id
      pure $ a & baseL . tokensL %~ setTokens Clue clueCount & baseL . withoutCluesL .~ (clueCount == 0)
    RemoveTokens _ (isTarget a -> True) token n -> pure $ a & baseL . tokensL %~ subtractTokens token n
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
    _ -> pure a
   where
    applyDamageMod (DamageDealt n) acc = acc + n
    applyDamageMod (DamageTaken n) acc = acc + n
    applyDamageMod NoDamageDealt _ = 0
    applyDamageMod _ acc = acc

-- | Determine if an investigator is allowed to investigate this enemy-location.
getInvestigateAllowed :: HasGame m => InvestigatorId -> EnemyLocationAttrs -> m Bool
getInvestigateAllowed _iid attrs = do
  modifiers' <- getModifiers (LocationTarget attrs.id)
  let cannotInvestigate = flip any modifiers' \case
        CannotInvestigateLocation lid -> lid == attrs.id
        CannotInvestigate -> True
        _ -> False
  pure $ not cannotInvestigate && isJust (locationShroud (enemyLocationBase attrs))
