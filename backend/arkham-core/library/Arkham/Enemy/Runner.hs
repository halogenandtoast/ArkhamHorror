{-# OPTIONS_GHC -Wno-orphans #-}
module Arkham.Enemy.Runner
  ( module Arkham.Enemy.Runner
  , module X
  ) where

import Arkham.Prelude

import Arkham.Enemy.Attrs as X
import Arkham.Enemy.Helpers as X hiding ( EnemyEvade, EnemyFight )
import Arkham.GameValue as X
import Arkham.Helpers.Enemy as X

import Arkham.Action qualified as Action
import Arkham.Attack
import Arkham.Card
import Arkham.Classes
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Investigator
import Arkham.Id
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
  ( AssetMatcher (..)
  , EnemyMatcher (..)
  , InvestigatorMatcher (..)
  , LocationMatcher (..)
  , PreyMatcher (..)
  , investigatorEngagedWith
  , locationWithInvestigator
  , preyWith
  )
import Arkham.Message
import Arkham.Message qualified as Msg
import Arkham.Placement
import Arkham.Projection
import Arkham.SkillType
import Arkham.Source
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Trait
import Arkham.Window ( Window (..) )
import Arkham.Window qualified as Window
import Data.List.Extra ( firstJust )
import Data.Monoid ( First (..) )

-- | Handle when enemy no longer exists
-- When an enemy is defeated we need to remove related messages from choices
-- and if not more choices exist, remove the message entirely
filterOutEnemyMessages :: EnemyId -> Message -> Maybe Message
filterOutEnemyMessages eid (Ask iid q) = case q of
  ChooseOne msgs -> case mapMaybe (filterOutEnemyMessages eid) msgs of
    [] -> Nothing
    x -> Just (Ask iid $ ChooseOne x)
  ChooseN n msgs -> case mapMaybe (filterOutEnemyMessages eid) msgs of
    [] -> Nothing
    x -> Just (Ask iid $ ChooseN n x)
  ChooseSome msgs -> case mapMaybe (filterOutEnemyMessages eid) msgs of
    [] -> Nothing
    x -> Just (Ask iid $ ChooseSome x)
  ChooseUpToN n msgs -> case mapMaybe (filterOutEnemyMessages eid) msgs of
    [] -> Nothing
    x -> Just (Ask iid $ ChooseUpToN n x)
  ChooseOneAtATime msgs -> case mapMaybe (filterOutEnemyMessages eid) msgs of
    [] -> Nothing
    x -> Just (Ask iid $ ChooseOneAtATime x)
  ChooseUpgradeDeck -> Just (Ask iid ChooseUpgradeDeck)
  choose@ChoosePaymentAmounts{} -> Just (Ask iid choose)
  choose@ChooseAmounts{} -> Just (Ask iid choose)
filterOutEnemyMessages eid msg = case msg of
  InitiateEnemyAttack _ eid' _ | eid == eid' -> Nothing
  EnemyAttack _ eid' _ _ | eid == eid' -> Nothing
  Discarded (EnemyTarget eid') _ | eid == eid' -> Nothing
  m -> Just m

getInvestigatorsAtSameLocation
  :: (Monad m, HasGame m) => EnemyAttrs -> m [InvestigatorId]
getInvestigatorsAtSameLocation attrs = do
  enemyLocation <- field EnemyLocation (toId attrs)
  case enemyLocation of
    Nothing -> pure []
    Just loc -> selectList $ InvestigatorAt $ LocationWithId loc

instance RunMessage EnemyAttrs where
  runMessage msg a@EnemyAttrs {..} = case msg of
    EndPhase -> pure $ a & movedFromHunterKeywordL .~ False
    SealedToken token card | toCardId card == toCardId a ->
      pure $ a & sealedTokensL %~ (token :)
    UnsealToken token -> pure $ a & sealedTokensL %~ filter (/= token)
    EnemySpawnEngagedWithPrey eid | eid == enemyId -> do
      preyIds <- selectList enemyPrey
      preyIdsWithLocation <- for preyIds
        $ traverseToSnd (selectJust . locationWithInvestigator)
      leadInvestigatorId <- getLeadInvestigatorId
      a <$ case preyIdsWithLocation of
        [] -> pure ()
        iids -> push
          (chooseOrRunOne
            leadInvestigatorId
            [ Run [EnemySpawnedAt lid eid, EnemyEngageInvestigator eid iid]
            | (iid, lid) <- iids
            ]
          )
    SetBearer (EnemyTarget eid) iid | eid == enemyId -> do
      pure $ a & bearerL ?~ iid
    EnemySpawn miid lid eid | eid == enemyId -> do
      locations' <- select Anywhere
      keywords <- getModifiedKeywords a
      if lid `notElem` locations'
        then a <$ push (Discard (EnemyTarget eid))
        else do
          if Keyword.Aloof
            `notElem` keywords
            && Keyword.Massive
            `notElem` keywords
            && not enemyExhausted
          then
            do
              preyIds <-
                selectList
                $ preyWith enemyPrey
                $ InvestigatorAt
                $ LocationWithId lid
              investigatorIds <- if null preyIds
                then selectList $ InvestigatorAt $ LocationWithId lid
                else pure []
              leadInvestigatorId <- getLeadInvestigatorId
              let
                validInvestigatorIds =
                  maybe (preyIds <> investigatorIds) pure miid
              case validInvestigatorIds of
                [] -> push $ EnemyEntered eid lid
                [iid] -> pushAll
                  [EnemyEntered eid lid, EnemyEngageInvestigator eid iid]
                iids -> push
                  (chooseOne
                    leadInvestigatorId
                    [ targetLabel
                        iid
                        [EnemyEntered eid lid, EnemyEngageInvestigator eid iid]
                    | iid <- iids
                    ]
                  )
          else
            when
              (Keyword.Massive `notElem` keywords)
              (push $ EnemyEntered eid lid)

          a <$ when
            (Keyword.Massive `elem` keywords)
            do
              investigatorIds <- selectList $ InvestigatorAt $ LocationWithId
                lid
              pushAll
                $ EnemyEntered eid lid
                : [ EnemyEngageInvestigator eid iid | iid <- investigatorIds ]
    EnemySpawnedAt lid eid | eid == enemyId -> do
      a <$ push (EnemyEntered eid lid)
    EnemyEntered eid lid | eid == enemyId -> do
      push =<< checkWindows
        ((`Window` Window.EnemyEnters eid lid) <$> [Timing.When, Timing.After])
      pure $ a & placementL .~ AtLocation lid
    Ready target | isTarget a target -> do
      leadInvestigatorId <- getLeadInvestigatorId
      enemyLocation <- field EnemyLocation enemyId
      iids <-
        fromMaybe []
          <$> traverse
                (selectList . InvestigatorAt . LocationWithId)
                enemyLocation
      keywords <- getModifiedKeywords a
      if null iids
        then pure ()
        else do
          unengaged <- selectNone $ investigatorEngagedWith enemyId
          when
              (Keyword.Aloof
              `notElem` keywords
              && (unengaged || Keyword.Massive `elem` keywords)
              )
            $ push
                (chooseOne
                  leadInvestigatorId
                  [ TargetLabel
                      (InvestigatorTarget iid)
                      [EnemyEngageInvestigator enemyId iid]
                  | iid <- iids
                  ]
                )
      pure $ a & exhaustedL .~ False
    ReadyExhausted -> do
      modifiers' <- getModifiers (toSource a) (toTarget a)
      let
        alternativeSources = mapMaybe
          (\case
            AlternativeReady source -> Just source
            _ -> Nothing
          )
          modifiers'
      case alternativeSources of
        [] -> a <$ when enemyExhausted (pushAll $ resolve (Ready $ toTarget a))
        [source] -> a <$ push (ReadyAlternative source (toTarget a))
        _ -> error "Can not handle multiple targets yet"
    MoveToward target locationMatcher | isTarget a target -> do
      enemyLocation <- field EnemyLocation enemyId
      case enemyLocation of
        Nothing -> pure a
        Just loc -> do
          lid <- fromJustNote "can't move toward" <$> selectOne locationMatcher
          if lid == loc
            then pure a
            else do
              leadInvestigatorId <- getLeadInvestigatorId
              adjacentLocationIds <-
                selectList $ AccessibleFrom $ LocationWithId loc
              closestLocationIds <- selectList $ ClosestPathLocation loc lid
              if lid `elem` adjacentLocationIds
                then
                  a <$ push
                    (chooseOne leadInvestigatorId [EnemyMove enemyId lid])
                else a <$ pushAll
                  [ chooseOne
                      leadInvestigatorId
                      [ EnemyMove enemyId lid' | lid' <- closestLocationIds ]
                  ]
    MoveUntil lid target | isTarget a target -> do
      enemyLocation <- field EnemyLocation enemyId
      case enemyLocation of
        Nothing -> pure a
        Just loc -> if lid == loc
          then pure a
          else do
            leadInvestigatorId <- getLeadInvestigatorId
            adjacentLocationIds <- selectList $ AccessibleFrom $ LocationWithId
              loc
            closestLocationIds <- selectList $ ClosestPathLocation loc lid
            if lid `elem` adjacentLocationIds
              then a
                <$ push (chooseOne leadInvestigatorId [EnemyMove enemyId lid])
              else a <$ pushAll
                [ chooseOne
                  leadInvestigatorId
                  [ EnemyMove enemyId lid' | lid' <- closestLocationIds ]
                , MoveUntil lid target
                ]
    EnemyMove eid lid | eid == enemyId -> do
      willMove <- canEnterLocation eid lid
      if willMove
        then do
          enemyLocation <- field EnemyLocation enemyId
          leaveWindows <- for enemyLocation
            $ \oldId -> windows [Window.EnemyLeaves eid oldId]
          pushAll
            $ fromMaybe [] leaveWindows
            <> [EnemyEntered eid lid, EnemyCheckEngagement eid]
          pure $ a & placementL .~ AtLocation lid
        else a <$ push (EnemyCheckEngagement eid)
    After (EndTurn _) -> a <$ push (EnemyCheckEngagement $ toId a)
    EnemyCheckEngagement eid | eid == enemyId -> do
      keywords <- getModifiedKeywords a
      modifiers' <- getModifiers (EnemySource eid) (EnemyTarget eid)
      let
        modifiedFilter = if Keyword.Massive `elem` keywords
          then const True
          else (`notElem` modifiers') . EnemyCannotEngage
      investigatorIds <- filter modifiedFilter
        <$> getInvestigatorsAtSameLocation a
      leadInvestigatorId <- getLeadInvestigatorId
      unengaged <- selectNone $ investigatorEngagedWith enemyId
      a <$ when
        (Keyword.Aloof
        `notElem` keywords
        && (unengaged || Keyword.Massive `elem` keywords)
        && not enemyExhausted
        )
        (if Keyword.Massive `elem` keywords
          then pushAll
            [ EnemyEngageInvestigator eid investigatorId
            | investigatorId <- investigatorIds
            ]
          else case investigatorIds of
            [] -> pure ()
            [x] -> push $ EnemyEngageInvestigator eid x
            xs -> push $ chooseOne
              leadInvestigatorId
              [ EnemyEngageInvestigator eid investigatorId
              | investigatorId <- xs
              ]
        )
    HuntersMove | not enemyExhausted -> do
      unengaged <- selectNone $ investigatorEngagedWith enemyId
      when unengaged $ do
        keywords <- getModifiedKeywords a
        leadInvestigatorId <- getLeadInvestigatorId
        when (Keyword.Hunter `elem` keywords) $ pushAll
          [ CheckWindow
            [leadInvestigatorId]
            [Window Timing.When (Window.MovedFromHunter enemyId)]
          , HunterMove (toId a)
          ]
      pure a
    HunterMove eid | eid == toId a && not enemyExhausted -> do
      enemyLocation <- field EnemyLocation enemyId
      case enemyLocation of
        Nothing -> pure a
        Just loc -> do
          modifiers' <- getModifiers (toSource a) (EnemyTarget enemyId)
          let
            matchForcedTargetLocation = \case
              DuringEnemyPhaseMustMoveToward (LocationTarget lid) -> Just lid
              _ -> Nothing
            forcedTargetLocation =
              firstJust matchForcedTargetLocation modifiers'
            -- applyConnectionMapModifier connectionMap (HunterConnectedTo lid') =
            --   unionWith (<>) connectionMap $ singletonMap loc [lid']
            -- applyConnectionMapModifier connectionMap _ = connectionMap
            -- extraConnectionsMap :: HashMap LocationId [LocationId] =
            --   foldl' applyConnectionMapModifier mempty modifiers'

          -- The logic here is an artifact of doing this incorrect
          -- Prey is only used for breaking ties unless we're dealing
          -- with the Only keyword for prey, so here we hardcode prey
          -- to AnyPrey and then find if there are any investigators
          -- who qualify as prey to filter
          matchingClosestLocationIds <-
            case (forcedTargetLocation, enemyPrey) of
              (Just _forcedTargetLocationId, _) -> error "TODO: MUST FIX"
                -- map unClosestPathLocationId <$> getSetList
                --   (loc, forcedTargetLocationId, extraConnectionsMap)
              (Nothing, BearerOf _) ->
                selectList $ locationWithInvestigator $ fromJustNote
                  "must have bearer"
                  enemyBearer
              (Nothing, OnlyPrey prey) ->
                selectList $ LocationWithInvestigator $ prey <> NearestToEnemy
                  (EnemyWithId eid)
              (Nothing, _prey) ->
                selectList
                  $ LocationWithInvestigator
                  $ NearestToEnemy
                  $ EnemyWithId eid

          preyIds <- select enemyPrey

          filteredClosestLocationIds <-
            flip filterM matchingClosestLocationIds $ \lid ->
              notNull . intersect preyIds <$> select
                (InvestigatorAt $ LocationWithId lid)

          -- If we have any locations with prey, that takes priority, otherwise
          -- we return all locations which may have matched via AnyPrey
          let
            destinationLocationIds = if null filteredClosestLocationIds
              then matchingClosestLocationIds
              else filteredClosestLocationIds

          leadInvestigatorId <- getLeadInvestigatorId
          pathIds <-
            concat
              <$> traverse
                    (selectList . ClosestPathLocation loc)
                    destinationLocationIds
          case pathIds of
            [] -> pure a
            [lid] -> do
              pushAll
                [ EnemyMove enemyId lid
                , CheckWindow
                  [leadInvestigatorId]
                  [Window Timing.After (Window.MovedFromHunter enemyId)]
                ]
              pure $ a & movedFromHunterKeywordL .~ True
            ls -> do
              pushAll
                (chooseOne
                    leadInvestigatorId
                    [ TargetLabel (LocationTarget l) [EnemyMove enemyId l]
                    | l <- ls
                    ]
                : [ CheckWindow
                      [leadInvestigatorId]
                      [Window Timing.After (Window.MovedFromHunter enemyId)]
                  ]
                )
              pure $ a & movedFromHunterKeywordL .~ True
    EnemiesAttack | not enemyExhausted -> do
      modifiers' <- getModifiers (toSource a) (EnemyTarget enemyId)
      unless (CannotAttack `elem` modifiers') $ do
        iids <- selectList $ investigatorEngagedWith enemyId
        pushAll $ map
          (\iid -> EnemyWillAttack iid enemyId enemyDamageStrategy RegularAttack
          )
          iids
      pure a
    AttackEnemy iid eid source mTarget skillType | eid == enemyId -> do
      enemyFight' <- modifiedEnemyFight a
      a <$ push
        (BeginSkillTest
          iid
          source
          (maybe (EnemyTarget eid) (ProxyTarget (EnemyTarget eid)) mTarget)
          (Just Action.Fight)
          skillType
          enemyFight'
        )
    PassedSkillTest iid (Just Action.Fight) source (SkillTestInitiatorTarget target) _ n
      | isActionTarget a target
      -> do
        whenWindow <- checkWindows
          [Window Timing.When (Window.SuccessfulAttackEnemy iid enemyId n)]
        afterSuccessfulWindow <- checkWindows
          [Window Timing.After (Window.SuccessfulAttackEnemy iid enemyId n)]
        afterWindow <- checkWindows
          [Window Timing.After (Window.EnemyAttacked iid source enemyId)]
        a <$ pushAll
          [ whenWindow
          , Successful
            (Action.Fight, toProxyTarget target)
            iid
            source
            (toActionTarget target)
            n
          , afterSuccessfulWindow
          , afterWindow
          ]
    Successful (Action.Fight, _) iid source target _ | isTarget a target -> do
      a <$ push (InvestigatorDamageEnemy iid enemyId source)
    FailedSkillTest iid (Just Action.Fight) source (SkillTestInitiatorTarget target) _ n
      | isTarget a target
      -> do
        keywords <- getModifiedKeywords a
        modifiers' <- getModifiers
          (InvestigatorSource iid)
          (InvestigatorTarget iid)
        a <$ pushAll
          ([ FailedAttackEnemy iid enemyId
           , CheckWindow
             [iid]
             [Window Timing.After (Window.FailAttackEnemy iid enemyId n)]
           , CheckWindow
             [iid]
             [Window Timing.After (Window.EnemyAttacked iid source enemyId)]
           ]
          <> [ EnemyAttack iid enemyId enemyDamageStrategy RegularAttack
             | Keyword.Retaliate
               `elem` keywords
               && IgnoreRetaliate
               `notElem` modifiers'
             ]
          )
    EnemyAttackIfEngaged eid miid | eid == enemyId -> do
      case miid of
        Just iid -> do
          shouldAttack <- member iid <$> select (investigatorEngagedWith eid)
          when shouldAttack $ push $ EnemyAttack
            iid
            enemyId
            enemyDamageStrategy
            RegularAttack
        Nothing -> do
          iids <- selectList $ investigatorEngagedWith eid
          pushAll
            [ EnemyAttack iid enemyId enemyDamageStrategy RegularAttack
            | iid <- iids
            ]
      pure a
    EnemyEvaded iid eid | eid == enemyId -> do
      lid <- getJustLocation iid
      pure $ a & placementL .~ AtLocation lid & exhaustedL .~ True
    TryEvadeEnemy iid eid source mTarget skillType | eid == enemyId -> do
      enemyEvade' <- modifiedEnemyEvade a
      a <$ push
        (BeginSkillTest
          iid
          source
          (maybe (EnemyTarget eid) (ProxyTarget (EnemyTarget eid)) mTarget)
          (Just Action.Evade)
          skillType
          enemyEvade'
        )
    PassedSkillTest iid (Just Action.Evade) source (SkillTestInitiatorTarget target) _ n
      | isActionTarget a target
      -> do
        whenWindow <- checkWindows
          [Window Timing.When (Window.SuccessfulEvadeEnemy iid enemyId n)]
        afterWindow <- checkWindows
          [Window Timing.After (Window.SuccessfulEvadeEnemy iid enemyId n)]
        a <$ pushAll
          [ whenWindow
          , Successful
            (Action.Evade, toProxyTarget target)
            iid
            source
            (toActionTarget target)
            n
          , afterWindow
          ]
    Successful (Action.Evade, _) iid _ target _ | isTarget a target -> do
      a <$ push (EnemyEvaded iid enemyId)
    FailedSkillTest iid (Just Action.Evade) _ (SkillTestInitiatorTarget target) _ n
      | isTarget a target
      -> do
        keywords <- getModifiedKeywords a
        whenWindow <- checkWindows
          [Window Timing.When (Window.FailEvadeEnemy iid enemyId n)]
        afterWindow <- checkWindows
          [Window Timing.After (Window.FailEvadeEnemy iid enemyId n)]
        a <$ pushAll
          ([whenWindow, afterWindow]
          <> [ EnemyAttack iid enemyId enemyDamageStrategy RegularAttack
             | Keyword.Alert `elem` keywords
             ]
          )
    InitiateEnemyAttack iid eid attackType | eid == enemyId -> do
      push $ EnemyAttack iid eid enemyDamageStrategy attackType
      pure a
    EnemyAttack iid eid damageStrategy attackType | eid == enemyId -> do
      whenAttacksWindow <- checkWindows
        [Window Timing.When (Window.EnemyAttacks iid eid attackType)]
      afterAttacksEventIfCancelledWindow <- checkWindows
        [Window Timing.When (Window.EnemyAttacksEvenIfCancelled iid eid attackType)]
      whenWouldAttackWindow <- checkWindows
        [Window Timing.When (Window.EnemyWouldAttack iid eid attackType)]
      a <$ pushAll
        [ whenWouldAttackWindow
        , whenAttacksWindow
        , PerformEnemyAttack iid eid damageStrategy attackType
        , After (PerformEnemyAttack iid eid damageStrategy attackType)
        , afterAttacksEventIfCancelledWindow
        ]
    PerformEnemyAttack iid eid damageStrategy attackType | eid == enemyId -> do
      modifiers <- getModifiers (EnemySource enemyId) (InvestigatorTarget iid)
      let
        validEnemyMatcher = foldl' applyModifiers AnyEnemy modifiers
        applyModifiers m (CancelAttacksByEnemies n) = m <> (NotEnemy n)
        applyModifiers m _ = m
      allowAttack <- member enemyId <$> select validEnemyMatcher
      pushAll
        $ [ InvestigatorAssignDamage
              iid
              (EnemyAttackSource enemyId)
              damageStrategy
              enemyHealthDamage
              enemySanityDamage
          | allowAttack
          ]
        <> [After (EnemyAttack iid enemyId damageStrategy attackType)]
      pure a
    HealDamage (EnemyTarget eid) n | eid == enemyId ->
      pure $ a & damageL %~ max 0 . subtract n
    HealAllDamage (EnemyTarget eid) | eid == enemyId -> pure $ a & damageL .~ 0
    EnemySetDamage eid _ amount | eid == enemyId -> pure $ a & damageL .~ amount
    Msg.EnemyDamage eid iid source damageEffect amount | eid == enemyId -> do
      canDamage <- sourceCanDamageEnemy eid source
      a <$ when
        canDamage
        do
          dealtDamageWhenMsg <- checkWindows
            [ Window
                Timing.When
                (Window.DealtDamage source damageEffect $ toTarget a)
            ]
          dealtDamageAfterMsg <- checkWindows
            [ Window
                Timing.After
                (Window.DealtDamage source damageEffect $ toTarget a)
            ]
          takeDamageWhenMsg <- checkWindows
            [ Window
                Timing.When
                (Window.TakeDamage source damageEffect $ toTarget a)
            ]
          takeDamageAfterMsg <- checkWindows
            [ Window
                Timing.After
                (Window.TakeDamage source damageEffect $ toTarget a)
            ]
          pushAll
            [ dealtDamageWhenMsg
            , dealtDamageAfterMsg
            , takeDamageWhenMsg
            , EnemyDamaged eid iid source damageEffect amount False
            , takeDamageAfterMsg
            ]
    DirectEnemyDamage eid iid source damageEffect amount | eid == enemyId -> do
      canDamage <- sourceCanDamageEnemy eid source
      a <$ when
        canDamage
        do
          dealtDamageWhenMsg <- checkWindows
            [ Window
                Timing.When
                (Window.DealtDamage source damageEffect $ toTarget a)
            ]
          dealtDamageAfterMsg <- checkWindows
            [ Window
                Timing.After
                (Window.DealtDamage source damageEffect $ toTarget a)
            ]
          takeDamageWhenMsg <- checkWindows
            [ Window
                Timing.When
                (Window.TakeDamage source damageEffect $ toTarget a)
            ]
          takeDamageAfterMsg <- checkWindows
            [ Window
                Timing.After
                (Window.TakeDamage source damageEffect $ toTarget a)
            ]
          pushAll
            [ dealtDamageWhenMsg
            , dealtDamageAfterMsg
            , takeDamageWhenMsg
            , EnemyDamaged eid iid source damageEffect amount True
            , takeDamageAfterMsg
            ]
    EnemyDamaged eid iid source damageEffect amount direct | eid == enemyId -> do
      amount' <- getModifiedDamageAmount a direct amount
      canBeDefeated <- withoutModifier a CannotBeDefeated
      modifiers' <- getModifiers (toSource a) (toTarget a)
      let
        canOnlyBeDefeatedByModifier = \case
          CanOnlyBeDefeatedBy source' -> First (Just source')
          _ -> First Nothing
        mOnlyBeDefeatedByModifier =
          getFirst $ foldMap canOnlyBeDefeatedByModifier modifiers'
        validDefeat =
          canBeDefeated && maybe True (== source) mOnlyBeDefeatedByModifier
      when validDefeat $ do
        modifiedHealth <- getModifiedHealth a
        when (a ^. damageL + amount' >= modifiedHealth) $ do
          let excess = (a ^. damageL + amount') - modifiedHealth
          whenMsg <- checkWindows
            [Window Timing.When (Window.EnemyWouldBeDefeated eid)]
          afterMsg <- checkWindows
            [Window Timing.After (Window.EnemyWouldBeDefeated eid)]
          whenExcessMsg <- checkWindows
            [Window Timing.When (Window.DealtExcessDamage source damageEffect (EnemyTarget eid) excess) | excess > 0]
          afterExcessMsg <- checkWindows
            [Window Timing.After (Window.DealtExcessDamage source damageEffect (EnemyTarget eid) excess) | excess > 0]
          pushAll
            [ whenExcessMsg
            , afterExcessMsg
            , whenMsg
            , afterMsg
            , EnemyDefeated
              eid
              iid
              (toCardCode a)
              source
              (setToList $ toTraits a)
            ]
      pure $ a & damageL +~ amount'
    DefeatEnemy eid iid source | eid == enemyId -> do
      canBeDefeated <- withoutModifier a CannotBeDefeated
      modifiedHealth <- getModifiedHealth a
      canOnlyBeDefeatedByDamage <- hasModifier a CanOnlyBeDefeatedByDamage
      modifiers' <- getModifiers (toSource a) (toTarget a)
      let
        defeatedByDamage = a ^. damageL >= modifiedHealth
        canOnlyBeDefeatedByModifier = \case
          CanOnlyBeDefeatedBy source' -> First (Just source')
          _ -> First Nothing
        mOnlyBeDefeatedByModifier =
          getFirst $ foldMap canOnlyBeDefeatedByModifier modifiers'
        validDefeat =
          canBeDefeated
            && maybe True (== source) mOnlyBeDefeatedByModifier
            && (not canOnlyBeDefeatedByDamage || defeatedByDamage)
      when validDefeat $ push $ EnemyDefeated
        eid
        iid
        (toCardCode a)
        source
        (setToList $ toTraits a)
      pure a
    EnemyDefeated eid iid _ _ _ | eid == toId a -> do
      whenMsg <- checkWindows
        [Window Timing.When (Window.EnemyDefeated iid eid)]
      afterMsg <- checkWindows
        [Window Timing.After (Window.EnemyDefeated iid eid)]
      enemyAssets <- selectList $ EnemyAsset eid
      let
        victory = cdVictoryPoints $ toCardDef a
        victoryMsgs = [ DefeatedAddToVictory $ toTarget a | isJust victory ]
        defeatMsgs =
          if isJust victory then [RemoveEnemy eid] else [Discard $ toTarget a]

      withQueue_ $ mapMaybe (filterOutEnemyMessages eid)

      a <$ pushAll
        (map (Discard . AssetTarget) enemyAssets
        <> [whenMsg, When msg, After msg]
        <> victoryMsgs
        <> [afterMsg]
        <> defeatMsgs
        )
    Discard target | a `isTarget` target -> do
      windows' <- windows [Window.WouldBeDiscarded (toTarget a)]
      a <$ pushAll
        (windows'
        <> [RemovedFromPlay $ toSource a, Discarded (toTarget a) (toCard a)]
        )
    PutOnTopOfEncounterDeck iid target | a `isTarget` target -> do
      case toCard a of
        EncounterCard ec -> do
          pushAll
            [ RemoveEnemy $ toId a
            , PutCardOnTopOfEncounterDeck iid ec
            ]
        PlayerCard _ ->
          error "Can not place player card on top of encounter deck"
      pure a
    RemovedFromPlay source | isSource a source -> do
      windowMsg <- checkWindows
        ((`Window` Window.LeavePlay (toTarget a))
        <$> [Timing.When, Timing.After]
        )
      pushAll $ windowMsg : [ UnsealToken token | token <- enemySealedTokens ]
      pure a
    EnemyEngageInvestigator eid iid | eid == enemyId -> do
      lid <- getJustLocation iid
      enemyLocation <- field EnemyLocation eid
      when (Just lid /= enemyLocation) (push $ EnemyEntered eid lid)
      pure $ a & placementL .~ InThreatArea iid
    EngageEnemy iid eid False | eid == enemyId ->
      -- TODO: Do we need to consider Massive here?
      pure $ a & placementL .~ InThreatArea iid
    WhenWillEnterLocation iid lid -> do
      shouldRespoond <- member iid <$> select (investigatorEngagedWith enemyId)
      when shouldRespoond $ do
        keywords <- getModifiedKeywords a
        willMove <- canEnterLocation enemyId lid
        if Keyword.Massive `notElem` keywords && willMove
          then push $ EnemyEntered enemyId lid
          else push $ DisengageEnemy iid enemyId
      pure a
    CheckAttackOfOpportunity iid isFast | not isFast && not enemyExhausted -> do
      willAttack <- member iid <$> select (investigatorEngagedWith enemyId)
      when willAttack $ do
        modifiers' <- getModifiers (toSource a) (EnemyTarget enemyId)
        unless (CannotMakeAttacksOfOpportunity `elem` modifiers')
          $ push
          $ EnemyWillAttack iid enemyId enemyDamageStrategy AttackOfOpportunity
      pure a
    InvestigatorDrawEnemy iid eid | eid == enemyId -> do
      lid <- getJustLocation iid
      modifiers' <- getModifiers (toSource a) (EnemyTarget enemyId)
      let
        getModifiedSpawnAt [] = enemySpawnAt
        getModifiedSpawnAt (SpawnLocation m : _) = Just m
        getModifiedSpawnAt (_ : xs) = getModifiedSpawnAt xs
        spawnAtMatcher = getModifiedSpawnAt modifiers'
      a
        <$ (case spawnAtMatcher of
             Nothing -> pushAll (resolve (EnemySpawn (Just iid) lid eid))
             Just matcher -> do
               spawnAt enemyId matcher
           )
    EnemySpawnAtLocationMatching miid locationMatcher eid | eid == enemyId -> do
      lids <- selectList locationMatcher
      leadInvestigatorId <- getLeadInvestigatorId
      a <$ case lids of
        [] ->
          pushAll
            $ Discard (EnemyTarget eid)
            : [ Surge iid (toSource a)
              | iid <- maybeToList miid
              , enemySurgeIfUnabledToSpawn
              ]
        [lid] -> pushAll (resolve $ EnemySpawn miid lid eid)
        xs -> spawnAtOneOf (fromMaybe leadInvestigatorId miid) eid xs
    InvestigatorEliminated iid -> case enemyPlacement of
      InThreatArea iid' | iid == iid' -> do
        lid <- getJustLocation iid
        pure $ a & placementL .~ AtLocation lid
      _ -> pure a
    DisengageEnemy iid eid | eid == enemyId -> case enemyPlacement of
      InThreatArea iid' | iid == iid' -> do
        lid <- getJustLocation iid
        pure $ a & placementL .~ AtLocation lid
      _ -> pure a
    DisengageEnemyFromAll eid | eid == enemyId -> case enemyPlacement of
      InThreatArea iid -> do
        lid <- getJustLocation iid
        pure $ a & placementL .~ AtLocation lid
      _ -> pure a
    AdvanceAgenda{} -> pure $ a & doomL .~ 0
    RemoveAllClues target | isTarget a target -> pure $ a & cluesL .~ 0
    RemoveAllDoom _ -> pure $ a & doomL .~ 0
    PlaceDoom target amount | isTarget a target -> pure $ a & doomL +~ amount
    RemoveDoom target amount | isTarget a target ->
      pure $ a & doomL %~ max 0 . subtract amount
    PlaceClues target n | isTarget a target -> do
      windows' <- windows [Window.PlacedClues (toTarget a) n]
      pushAll windows'
      pure $ a & cluesL +~ n
    RemoveClues target n | isTarget a target ->
      pure $ a & cluesL %~ max 0 . subtract n
    FlipClues target n | isTarget a target -> do
      let flipCount = min n enemyClues
      pure $ a & cluesL %~ max 0 . subtract n & doomL +~ flipCount
    PlaceEnemyInVoid eid | eid == enemyId -> do
      withQueue_ $ mapMaybe (filterOutEnemyMessages eid)
      pure
        $ a
        & (damageL .~ 0)
        & (placementL .~ TheVoid)
        & (exhaustedL .~ False)
        & (doomL .~ 0)
        & (cluesL .~ 0)
    Blanked msg' -> runMessage msg' a
    UseCardAbility iid source _ 100 _ | isSource a source -> a <$ push
      (FightEnemy
        iid
        (toId a)
        (InvestigatorSource iid)
        Nothing
        SkillCombat
        False
      )
    UseCardAbility iid source _ 101 _ | isSource a source -> a <$ push
      (EvadeEnemy
        iid
        (toId a)
        (InvestigatorSource iid)
        Nothing
        SkillAgility
        False
      )
    UseCardAbility iid source _ 102 _ | isSource a source ->
      a <$ push (EngageEnemy iid (toId a) False)
    _ -> pure a
