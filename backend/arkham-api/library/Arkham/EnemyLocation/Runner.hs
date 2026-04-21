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
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Constants
import Arkham.DamageEffect (DamageAssignment (..))
import Arkham.DefeatedBy
import Arkham.Fight
import Arkham.Helpers.Source (getSourceController)
import Arkham.Helpers.GameValue (getGameValue)
import Arkham.History
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Window (checkAfter, checkWhen, checkWindows, frame)
import Arkham.Investigate
import Arkham.Location.Grid
import Arkham.Matcher (LocationMatcher (..), pattern YourLocation)
import Arkham.Name (display, toName)
import Arkham.Message
import Arkham.Message qualified as Msg
import Arkham.Prelude
import Arkham.Token
import Arkham.Trait
import Arkham.Window qualified as Window

-- | The coerced EnemyId used for fight/evade targeting by the enemy subsystem.
asEnemyId :: EnemyLocationAttrs -> EnemyId
asEnemyId = enemyLocationAsEnemyId

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
        $ investigateAbility a AbilityInvestigate mempty (OnSameLocation <> exists (YourLocation <> InvestigatableLocation))
    ]

instance RunMessage EnemyLocationAttrs where
  runMessage msg a@EnemyLocationAttrs {..} = runQueueT $ case msg of
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
    -- Successful evade: exhaust isn't applicable for locations, but we record the evade.
    PassedSkillTest iid (Just Action.Evade) source (Initiator target) _ n | isEnemyTarget a target -> do
      let eid = asEnemyId a
      whenMsg <- checkWhen $ Window.SuccessfulEvadeEnemy iid source eid n
      afterMsg <- checkAfter $ Window.SuccessfulEvadeEnemy iid source eid n
      pushAll [whenMsg, EnemyEvaded iid eid, afterMsg]
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
getInvestigateAllowed :: (HasGame m) => InvestigatorId -> EnemyLocationAttrs -> m Bool
getInvestigateAllowed _iid attrs = do
  modifiers' <- getModifiers (LocationTarget attrs.id)
  let cannotInvestigate = flip any modifiers' \case
        CannotInvestigateLocation lid -> lid == attrs.id
        CannotInvestigate -> True
        _ -> False
  pure $ not cannotInvestigate
