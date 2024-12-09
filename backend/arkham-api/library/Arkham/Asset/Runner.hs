{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Asset.Runner (
  module X,
  hasUses,
) where

import Arkham.Prelude

import Arkham.Asset.Helpers as X hiding (defeated, getMeta)
import Arkham.Asset.Types as X
import Arkham.Asset.Uses as X
import Arkham.Calculation as X
import Arkham.Classes as X
import Arkham.GameValue as X
import Arkham.Helpers.Effect as X
import Arkham.Helpers.Message as X hiding (RevealChaosToken)
import Arkham.Helpers.SkillTest as X
import Arkham.SkillTest.Base as X (SkillTestDifficulty (..))
import Arkham.Source as X
import Arkham.Target as X

import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes.HasGame
import Arkham.Damage
import Arkham.DamageEffect
import Arkham.DefeatedBy
import Arkham.Event.Types (Field (EventUses))
import Arkham.Helpers.Calculation (calculate)
import Arkham.Helpers.Customization
import Arkham.Helpers.Placement
import Arkham.Helpers.Use
import Arkham.Investigator.Types (Field (InvestigatorRemainingHealth, InvestigatorRemainingSanity))
import Arkham.Matcher (
  AssetMatcher (AnyAsset, AssetAttachedToAsset, AssetWithId),
  EventMatcher (EventAttachedToAsset),
 )
import Arkham.Message qualified as Msg
import Arkham.Projection
import Arkham.Timing qualified as Timing
import Arkham.Token qualified as Token
import Arkham.Window (mkAfter, mkWhen, mkWindow)
import Arkham.Window qualified as Window
import Arkham.Zone qualified as Zone
import Control.Lens (non)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map

defeated :: HasGame m => AssetAttrs -> Source -> m (Maybe DefeatedBy)
defeated AssetAttrs {assetId, assetAssignedHealthDamage, assetAssignedSanityDamage} source = do
  remainingHealth <- field AssetRemainingHealth assetId
  remainingSanity <- field AssetRemainingSanity assetId
  pure $ case (remainingHealth, remainingSanity) of
    (Just a, Just b)
      | a - assetAssignedHealthDamage <= 0 && b - assetAssignedSanityDamage <= 0 ->
          Just (DefeatedByDamageAndHorror source)
    (Just a, _) | a - assetAssignedHealthDamage <= 0 -> Just (DefeatedByDamage source)
    (_, Just b) | b - assetAssignedSanityDamage <= 0 -> Just (DefeatedByHorror source)
    _ -> Nothing

hasUses :: AssetAttrs -> Bool
hasUses = any (> 0) . toList . assetUses

instance RunMessage Asset where
  runMessage msg x@(Asset a) = do
    inPlay <- elem (toId x) <$> select AnyAsset
    modifiers' <- if inPlay then getModifiers (toTarget x) else pure []
    let msg' = if any (`elem` modifiers') [Blank, BlankExceptForcedAbilities] then Blanked msg else msg
    Asset <$> runMessage msg' a

instance RunMessage AssetAttrs where
  runMessage msg a@AssetAttrs {..} = case msg of
    SetDriver aid iid | aid == assetId -> do
      pure $ a & driverL ?~ iid
    Msg.DealAssetDamageWithCheck aid source damage horror doCheck | aid == assetId -> do
      case a.controller of
        Nothing -> runMessage (Msg.AssignAssetDamageWithCheck aid source damage horror doCheck) a
        Just iid -> do
          mods <- getModifiers aid

          damageHank <-
            filterM
              (fieldP InvestigatorRemainingHealth (> 0))
              [x | damage > 0, CanAssignDamageToInvestigator x <- mods]
          horrorHank <-
            filterM
              (fieldP InvestigatorRemainingSanity (> 0))
              [x | horror > 0, CanAssignDamageToInvestigator x <- mods]

          if null damageHank && null horrorHank
            then runMessage (Msg.AssignAssetDamageWithCheck aid source damage horror doCheck) a
            else do
              -- since we need to assign we'll disable doCheck on subsequent calls, so we need to queue the check now
              pushAll [checkDefeated source aid | doCheck]
              let
                assignRestOfHealthDamage =
                  Msg.DealAssetDamageWithCheck aid source (damage - 1) horror False
                assignRestOfSanityDamage =
                  Msg.DealAssetDamageWithCheck aid source damage (horror - 1) False
                damageAsset =
                  AssetDamageLabel
                    aid
                    [ Msg.AssignAssetDamageWithCheck aid source 1 0 False
                    , assignRestOfHealthDamage
                    ]
                damageInvestigator iid' =
                  DamageLabel
                    iid'
                    [ Msg.InvestigatorDamage iid' source 1 0
                    , assignRestOfHealthDamage
                    ]
                horrorAsset =
                  AssetHorrorLabel
                    aid
                    [ Msg.AssignAssetDamageWithCheck aid source 1 0 False
                    , assignRestOfSanityDamage
                    ]
                horrorInvestigator iid' =
                  HorrorLabel
                    iid'
                    [ Msg.InvestigatorDamage iid' source 1 0
                    , assignRestOfSanityDamage
                    ]
              player <- getPlayer iid
              push
                $ chooseOne player
                $ [damageAsset | damage > 0]
                <> [horrorAsset | horror > 0]
                <> [damageInvestigator iid' | damage > 0, iid' <- damageHank]
                <> [horrorInvestigator iid' | horror > 0, iid' <- damageHank]
              pure a
    Msg.DealAssetDirectDamage aid source damage horror | aid == assetId -> do
      mods <- getModifiers a
      let n = sum [x | DamageTaken x <- mods]
      let
        damageEffect = case source of
          EnemyAttackSource _ -> AttackDamageEffect
          _ -> NonAttackDamageEffect
      pushAll
        $ [PlaceDamage source (toTarget a) (damage + n) | damage > 0]
        <> [PlaceHorror source (toTarget a) horror | horror > 0]
        <> [ CheckWindows
              $ [ mkWhen (Window.DealtDamage source damageEffect (toTarget a) damage)
                | damage > 0
                ]
              <> [ mkWhen (Window.DealtHorror source (toTarget a) horror)
                 | horror > 0
                 ]
           , checkDefeated source aid
           , CheckWindows
              $ [ mkAfter (Window.DealtDamage source damageEffect (toTarget a) damage)
                | damage > 0
                ]
              <> [ mkAfter (Window.DealtHorror source (toTarget a) horror)
                 | horror > 0
                 ]
           ]
      pure a
    Msg.AssignAssetDamageWithCheck aid source damage horror doCheck | aid == assetId -> do
      mods <- getModifiers a
      let n = sum [x | DamageTaken x <- mods]
          extraHealth = sum [x | HealthModifier x <- mods]
          extraSanity = sum [x | SanityModifier x <- mods]
      let damage' = maybe 0 (min (damage + n) . subtract (assetDamage a) . (+ extraHealth)) assetHealth
      let horror' = maybe 0 (min horror . subtract (assetHorror a) . (+ extraSanity)) assetSanity
      if doCheck
        then push $ Msg.DealAssetDirectDamage aid source damage' horror'
        else
          pushAll
            $ [PlaceDamage source (toTarget a) damage' | damage' > 0]
            <> [PlaceHorror source (toTarget a) horror' | horror' > 0]
      pure a
    IncreaseCustomization iid cardCode customization choices | toCardCode a == cardCode && a `ownedBy` iid -> do
      case customizationIndex a customization of
        Nothing -> pure a
        Just i ->
          pure
            $ a {assetCustomizations = IntMap.adjust (second (const choices) . first (+ 1)) i assetCustomizations}
    SetOriginalCardCode cardCode -> pure $ a & originalCardCodeL .~ cardCode
    SealedChaosToken token (isTarget a -> True) -> do
      pure $ a & sealedChaosTokensL %~ (token :)
    SealedChaosToken token _ -> do
      pure $ a & sealedChaosTokensL %~ filter (/= token)
    UnsealChaosToken token -> pure $ a & sealedChaosTokensL %~ filter (/= token)
    ReturnChaosTokensToPool tokens -> pure $ a & sealedChaosTokensL %~ filter (`notElem` tokens)
    RemoveAllChaosTokens face -> do
      pure $ a & sealedChaosTokensL %~ filter ((/= face) . chaosTokenFace)
    ReadyExhausted -> do
      case a.controller of
        Just iid -> do
          modifiers <- getModifiers iid
          pushWhen (ControlledAssetsCannotReady `notElem` modifiers) (Ready $ toTarget a)
        _ -> push (Ready $ toTarget a)
      pure a
    RemoveAllDoom _ target | isTarget a target -> pure $ a & tokensL %~ removeAllTokens Doom
    PlaceTokens source target tType n | isTarget a target -> do
      pushM $ checkAfter $ Window.PlacedToken source target tType n
      when (tType == Doom && a.doom == 0) do
        pushM $ checkAfter $ Window.PlacedDoomCounterOnTargetWithNoDoom source target n
      if tokenIsUse tType
        then case assetPrintedUses of
          NoUses -> pure $ a & tokensL . at tType . non 0 %~ (+ n)
          Uses useType'' _ | tType == useType'' -> do
            pure $ a & tokensL . at tType . non 0 %~ (+ n)
          UsesWithLimit useType'' _ pl | tType == useType'' -> do
            l <- calculate pl
            pure $ a & tokensL . at tType . non 0 %~ min l . (+ n)
          _ ->
            error
              $ "Trying to add the wrong use type, has "
              <> show assetPrintedUses
              <> ", but got: "
              <> show tType
        else do
          mods <- getModifiers a
          let mSpirit = listToMaybe [iid | IsSpirit iid <- mods]
          case mSpirit of
            Just iid | tType `elem` [#damage, #horror] -> do
              push $ PlaceTokens source (toTarget iid) tType n
              pure a
            _ -> pure $ a & tokensL %~ addTokens tType n
    MoveTokens s source _ tType n | isSource a source -> do
      runMessage (RemoveTokens s (toTarget a) tType n) a
    MoveTokens s _ target tType n | isTarget a target -> do
      a' <- runMessage (PlaceTokens s (toTarget a) tType n) a
      pushWhen (tType `elem` [Horror, Damage]) $ checkDefeated s a'
      pure a'
    MoveTokensNoDefeated s _ target tType n | isTarget a target -> do
      runMessage (PlaceTokens s (toTarget a) tType n) a
    MoveTokensNoDefeated s source _ tType n | isSource a source -> do
      runMessage (RemoveTokens s (toTarget a) tType n) a
    ClearTokens target | isTarget a target -> do
      when (assetClues a > 0) do
        pushAll $ windows [Window.LastClueRemovedFromAsset (toId a)]
      for_ assetWhenNoUses \case
        DiscardWhenNoUses -> push $ Discard assetController GameSource (toTarget a)
        ReturnToHandWhenNoUses ->
          for_ assetController \iid ->
            push $ ReturnToHand iid $ toTarget a
        NotifySelfOfNoUses -> push $ SpentAllUses (toTarget a)
      pure $ a & tokensL .~ mempty
    RemoveTokens _ target tType n | isTarget a target -> do
      when (tType == Clue && assetClues a - n <= 0) do
        pushAll $ windows [Window.LastClueRemovedFromAsset (toId a)]
      when (tokenIsUse tType) do
        case assetPrintedUses of
          NoUses -> pure ()
          Uses tType' _ | tType == tType' -> do
            let m = findWithDefault 0 tType assetTokens
            let remainingUses = max 0 (m - n)
            when (remainingUses == 0) $ for_ assetWhenNoUses \case
              DiscardWhenNoUses -> push $ Discard assetController GameSource (toTarget a)
              ReturnToHandWhenNoUses ->
                for_ assetController \iid ->
                  push $ ReturnToHand iid $ toTarget a
              NotifySelfOfNoUses -> push $ SpentAllUses (toTarget a)
          UsesWithLimit tType' _ _ | tType == tType' -> do
            let m = findWithDefault 0 tType assetTokens
            let remainingUses = max 0 (m - n)
            when (remainingUses == 0) $ for_ assetWhenNoUses \case
              DiscardWhenNoUses -> push $ Discard assetController GameSource (toTarget a)
              ReturnToHandWhenNoUses ->
                for_ assetController \iid ->
                  push $ ReturnToHand iid $ toTarget a
              NotifySelfOfNoUses -> push $ SpentAllUses (toTarget a)
          _ -> error "Trying to use the wrong use type"
      pure $ a & tokensL %~ subtractTokens tType n
    CheckDefeated source (isTarget a -> True) -> do
      mDefeated <- defeated a source
      for_ mDefeated \defeatedBy -> do
        let (before, _, after) = frame (Window.AssetDefeated (toId a) defeatedBy)
        pushAll $ [before] <> resolve (AssetDefeated source assetId) <> [after]
      -- TODO: Investigator uses AssignDamage target
      pure
        $ a
        & ( tokensL
              %~ (addTokens #damage assetAssignedHealthDamage . addTokens #horror assetAssignedSanityDamage)
          )
        & (assignedHealthDamageL .~ 0)
        & (assignedSanityDamageL .~ 0)
    CancelAssetDamage aid _ n | aid == assetId -> do
      pushM $ checkAfter $ Window.CancelledOrIgnoredCardOrGameEffect (toSource a)
      pure $ a & tokensL %~ decrementTokensBy Token.Damage n
    AssetDefeated source aid | aid == assetId -> do
      push $ toDiscard source a
      pure a
    ReassignHorror source (isTarget a -> True) n -> do
      alreadyChecked <- assertQueue \case
        CheckDefeated _ target -> target == toTarget a
        _ -> False
      unless alreadyChecked do
        replaceMessageMatching
          \case
            CheckDefeated _ target -> target == sourceToTarget source
            _ -> False
          \case
            msg'@(CheckDefeated s _) -> [msg', CheckDefeated s (toTarget a)]
            _ -> error "Invalid match"

      pure $ a & assignedSanityDamageL +~ n
    ApplyHealing source -> do
      let health = findWithDefault 0 source assetAssignedHealthHeal
      let sanity = findWithDefault 0 source assetAssignedSanityHeal
      when (health > 0 || sanity > 0) do
        pushM
          $ checkWindows
          $ [mkWindow Timing.After (Window.Healed DamageType (toTarget a) source health) | health > 0]
          <> [mkWindow Timing.After (Window.Healed DamageType (toTarget a) source sanity) | sanity > 0]
      pure $ a & tokensL %~ subtractTokens Token.Damage health . subtractTokens Token.Horror sanity
    HealDamage (isTarget a -> True) source amount -> do
      mods <- getModifiers a
      let n = sum [x | HealingTaken x <- mods]
      let amount' = amount + n
      afterWindow <- checkWindows [mkWindow Timing.After (Window.Healed DamageType (toTarget a) source n)]
      push $ AssignedHealing (toTarget a)
      push afterWindow
      runMessage (RemoveTokens source (toTarget a) Token.Damage amount') a
    HealDamageDelayed (isTarget a -> True) source amount -> do
      mods <- getModifiers a
      let n = sum [x | HealingTaken x <- mods]
      let amount' = amount + n
      push $ AssignedHealing (toTarget a)
      pure $ a & assignedHealthHealL %~ insertWith (+) source amount'
    HealHorror (isTarget a -> True) source amount -> do
      mods <- getModifiers a
      let n = sum [x | HealingTaken x <- mods]
      let amount' = amount + n
      push $ AssignedHealing (toTarget a)
      afterWindow <- checkWindows [mkWindow Timing.After (Window.Healed HorrorType (toTarget a) source n)]
      push afterWindow
      runMessage (RemoveTokens source (toTarget a) Token.Horror amount') a
    HealHorrorDelayed (isTarget a -> True) source amount -> do
      mods <- getModifiers a
      let n = sum [x | HealingTaken x <- mods]
      let amount' = amount + n
      push $ AssignedHealing (toTarget a)
      pure $ a & assignedSanityHealL %~ insertWith (+) source amount'
    HealHorrorDirectly target source amount | isTarget a target -> do
      -- USE ONLY WHEN NO CALLBACKS
      mods <- getModifiers a
      let n = sum [x | HealingTaken x <- mods]
      let amount' = amount + n
      push $ AssignedHealing (toTarget a)
      runMessage (RemoveTokens source (toTarget a) Token.Horror amount') a
    HealDamageDirectly target source amount | isTarget a target -> do
      -- USE ONLY WHEN NO CALLBACKS
      mods <- getModifiers a
      let n = sum [x | HealingTaken x <- mods]
      let amount' = amount + n
      push $ AssignedHealing (toTarget a)
      runMessage (RemoveTokens source (toTarget a) Token.Damage amount') a
    When (InvestigatorResigned iid) -> do
      let
        shouldResignWith = case assetPlacement of
          InPlayArea iid' -> iid == iid'
          InThreatArea iid' -> iid == iid'
          AttachedToInvestigator iid' -> iid == iid'
          _ -> False
      pushWhen shouldResignWith $ ResignWith (AssetTarget assetId)
      pure a
    InvestigatorEliminated iid -> do
      let
        shouldDiscard = case assetPlacement of
          InPlayArea iid' -> iid == iid'
          InThreatArea iid' -> iid == iid'
          AttachedToInvestigator iid' -> iid == iid'
          _ -> a.controller == Just iid
      pushWhen shouldDiscard $ RemoveFromGame (toTarget a)
      pure a
    AddUses source aid useType' n | aid == assetId -> runMessage (PlaceTokens source (toTarget a) useType' n) a
    SpendUses source target useType' n | isTarget a target -> do
      mods <- getModifiers a
      otherSourcePairs <- forMaybeM mods \case
        ProvidesUses uType s | uType == useType' -> do
          case s of
            AssetSource aid' -> do
              uses <- fieldMap AssetUses (findWithDefault 0 useType') aid'
              pure $ Just (AssetTarget aid', uType, uses)
            EventSource eid' -> do
              uses <- fieldMap EventUses (findWithDefault 0 useType') eid'
              pure $ Just (EventTarget eid', uType, uses)
            _ -> error $ "Unhandled source: " <> show source
        ProvidesProxyUses pType uType s | uType == useType' -> do
          case s of
            AssetSource aid' -> do
              uses <- fieldMap AssetUses (findWithDefault 0 pType) aid'
              pure $ Just (AssetTarget aid', pType, uses)
            EventSource eid' -> do
              uses <- fieldMap EventUses (findWithDefault 0 pType) eid'
              pure $ Just (EventTarget eid', pType, uses)
            _ -> error $ "Unhandled source: " <> show source
        _ -> pure Nothing

      if null otherSourcePairs
        then push $ Do msg
        else case a.controller of
          Nothing -> error "Cannot spend uses on an asset with no controller"
          Just iid -> do
            -- we may want a nicer way to handle this, but for the now the
            -- logic is to duplicate the choice for each use (the ui can only
            -- display it being clickable) and then to remove that choice from
            -- the list when used.
            player <- getPlayer iid
            push
              $ chooseN player n
              $ replicate (a.use useType') (targetLabel a [Do msg])
              <> concat
                [ replicate x (targetLabel otherTarget [Do $ SpendUses source otherTarget actualUseType n])
                | (otherTarget, actualUseType, x) <- otherSourcePairs
                ]
      pure a
    Do (SpendUses source target useType' n) | isTarget a target -> do
      when (n >= a.use useType') do
        -- N.B. we either need to find the end of the attack and insert this
        -- when it ends or an effect. This window is currently only specific to
        -- One in the Chamber and this might not be exhaustive so if a bug
        -- comes in this could be a likely culprit. It might also have to do
        -- with when we actually issue the resolved play card message
        afterLast <- checkAfter $ Window.AttackOrEffectSpentLastUse source (toTarget a) useType'
        case source of
          AbilitySource s i -> insertAfterMatchingOrNow [MoveWithSkillTest afterLast] \case
            ResolvedAbility ab -> ab.source == s && ab.index == i
            MoveWithSkillTest (ResolvedAbility ab) -> ab.source == s && ab.index == i
            MovedWithSkillTest _ (ResolvedAbility ab) -> ab.source == s && ab.index == i
            _ -> False
          CardIdSource cid -> insertAfterMatchingOrNow [MoveWithSkillTest afterLast] \case
            ResolvedPlayCard _ c' -> cid == c'.id
            ResolvedAbility _ -> True
            MoveWithSkillTest (ResolvedAbility _) -> True
            MovedWithSkillTest _ (ResolvedAbility _) -> True
            _ -> False
          EventSource e -> do
            mAbility <- findFromQueue \case
              ResolvedAbility _ -> True
              MoveWithSkillTest (ResolvedAbility _) -> True
              MovedWithSkillTest _ (ResolvedAbility _) -> True
              _ -> False
            case mAbility of
              Nothing -> insertAfterMatchingOrNow [MoveWithSkillTest afterLast] \case
                FinishedEvent e' -> e == e'
                _ -> False
              Just msg' -> insertAfterMatchingOrNow [MoveWithSkillTest afterLast] (== msg')
          _ -> pure ()
      runQueueT do
        let (before, _, after) = frame $ Window.SpentToken source (toTarget a) useType' n
        -- window should be independent of other sources since they are spent from this asset
        push before
        for_ assetController $ \controller ->
          when (tokenIsUse useType') do
            let (before1, _, after1) = frame $ Window.SpentUses controller source (toId a) useType' n
            push before1
            push after1
        push after

      runMessage (RemoveTokens source target useType' n) a
    AttachAsset aid target | aid == assetId -> do
      case target of
        LocationTarget lid -> push $ PlaceAsset aid (AttachedToLocation lid)
        EnemyTarget eid -> push $ PlaceAsset aid (AttachedToEnemy eid)
        _ -> error "Cannot attach asset to that type"
      pure a
    RemoveFromGame target | a `isTarget` target -> do
      a <$ push (RemoveFromPlay $ toSource a)
    Discard mInvestigator source target | a `isTarget` target -> do
      removeFromGame <- a `hasModifier` RemoveFromGameInsteadOfDiscard
      afterWindows <- checkAfter $ Window.Discarded mInvestigator source (toCard a)
      let discardMsg = if removeFromGame then RemoveFromGame (toTarget a) else Discarded (toTarget a) source (toCard a)
      pushAll
        $ windows [Window.WouldBeDiscarded (toTarget a)]
        <> [RemoveFromPlay $ toSource a, discardMsg, afterWindows]
      for_ a.cardsUnderneath $ push . DiscardedCard . toCardId
      pure a
    Exile target | a `isTarget` target -> do
      pushAll [RemoveFromPlay $ toSource a, Exiled target (toCard a)]
      pure $ a & exiledL .~ True
    RemoveFromPlay source | isSource a source -> do
      attachedAssets <- select $ AssetAttachedToAsset $ AssetWithId (toId a)
      attachedEvents <- select $ EventAttachedToAsset $ AssetWithId (toId a)
      windowMsg <-
        checkWindows
          ( (`mkWindow` Window.LeavePlay (toTarget a))
              <$> [Timing.When, Timing.AtIf, Timing.After]
          )
      pushAll
        $ windowMsg
        : [UnsealChaosToken token | token <- assetSealedChaosTokens]
          <> [Discard Nothing GameSource (toTarget a') | a' <- attachedAssets]
          <> [Discard Nothing GameSource (toTarget a') | a' <- attachedEvents]
          <> [RemovedFromPlay source]
      pure a
    PlaceInBonded _iid card -> do
      when (toCard a == card) do
        removeAllMessagesMatching \case
          Discarded (AssetTarget aid) _ _ -> aid == a.id
          CheckWindows ws -> flip any ws \case
            (Window.windowType -> Window.Discarded _ _ c) -> toCard a == c
            _ -> False
          Do (CheckWindows ws) -> flip any ws \case
            (Window.windowType -> Window.Discarded _ _ c) -> toCard a == c
            _ -> False
          _ -> False
        push $ RemoveFromGame (toTarget a)
      pure a
    RemovedFromPlay (isSource a -> True) -> do
      pure $ a & placementL .~ OutOfPlay Zone.RemovedZone
    PlaceKey (isTarget a -> True) k -> do
      pure $ a & (keysL %~ insertSet k)
    HealAllDamage (isTarget a -> True) source | assetDamage a > 0 -> do
      afterWindow <- checkWindows [mkAfter $ Window.Healed #damage (toTarget a) source (assetDamage a)]
      push afterWindow
      pure $ a & tokensL %~ removeAllTokens Token.Damage
    HealAllHorror (isTarget a -> True) source | assetHorror a > 0 -> do
      afterWindow <- checkWindows [mkAfter $ Window.Healed #horror (toTarget a) source (assetHorror a)]
      push afterWindow
      pure $ a & tokensL %~ removeAllTokens Token.Horror
    HealAllDamageAndHorror (isTarget a -> True) source | assetDamage a > 0 || assetHorror a > 0 -> do
      afterWindow <-
        checkWindows
          $ [mkAfter $ Window.Healed #damage (toTarget a) source (assetDamage a) | assetDamage a > 0]
          <> [mkAfter $ Window.Healed #horror (toTarget a) source (assetHorror a) | assetHorror a > 0]
      push afterWindow
      pure $ a & tokensL %~ removeAllTokens Token.Horror & tokensL %~ removeAllTokens Token.Damage
    InvestigatorPlayedAsset iid aid | aid == assetId -> do
      let placement = if isInPlayPlacement a.placement then a.placement else InPlayArea iid
      runMessage (PlaceAsset aid placement) a
    TakeControlOfAsset iid aid | aid == assetId -> do
      pushM $ checkWindows $ (`mkWindow` Window.TookControlOfAsset iid aid) <$> [#when, #after]
      pure $ a & placementL .~ InPlayArea iid & controllerL ?~ iid
    LoseControlOfAsset aid | aid == assetId -> do
      pure $ a & controllerL .~ Nothing
    ReplacedInvestigatorAsset iid aid | aid == assetId -> do
      pure $ a & placementL .~ InPlayArea iid & controllerL ?~ iid
    AddToScenarioDeck key target | isTarget a target -> do
      pushAll
        [AddCardToScenarioDeck key (toCard a), RemoveFromGame (toTarget a)]
      pure $ a & placementL .~ Unplaced
    ShuffleCardsIntoDeck _ cards ->
      pure $ a & cardsUnderneathL %~ filter (`notElem` cards)
    ShuffleIntoDeck _ (isTarget a -> True) -> do
      removeAllMessagesMatching \case
        When (AssetDefeated _ aid) -> aid == assetId
        AssetDefeated _ aid -> aid == assetId
        Discard _ _ (AssetTarget aid) -> aid == assetId
        _ -> False

      pure a
    CardEnteredPlay _ card ->
      pure $ a & cardsUnderneathL %~ filter (/= card)
    Exhaust target | a `isTarget` target -> do
      pushAll $ doFrame (Exhaust target) (Window.Exhausts (toTarget a))
      pure a
    Do (Exhaust target) | a `isTarget` target -> do
      pure $ a & exhaustedL .~ True
    ExhaustThen target msgs | a `isTarget` target -> do
      unless assetExhausted $ pushAll msgs
      pure $ a & exhaustedL .~ True
    Ready target | a `isTarget` target -> case a.controller of
      Just iid -> do
        modifiers <- getModifiers (InvestigatorTarget iid)
        if ControlledAssetsCannotReady `elem` modifiers
          then pure a
          else pure $ a & exhaustedL .~ False
      Nothing -> pure $ a & exhaustedL .~ False
    PlaceUnderneath (isTarget a -> True) cards -> do
      pure $ a & cardsUnderneathL <>~ cards
    PlaceUnderneath _ cards | toCard a `elem` cards -> do
      push $ RemoveFromPlay (toSource a)
      pure a
    AddToDiscard _ c -> do
      pure $ a & cardsUnderneathL %~ filter (/= toCard c)
    AddToEncounterDiscard c -> do
      pure $ a & cardsUnderneathL %~ filter (/= toCard c)
    CommitCard _ card -> do
      pure $ a & cardsUnderneathL %~ filter (/= card)
    AddToHand _ cards -> do
      pure $ a & cardsUnderneathL %~ filter (`notElem` cards)
    InvestigatorDrewPlayerCardFrom _ card _ -> do
      pure $ a & cardsUnderneathL %~ filter (/= toCard card)
    InvestigatorDrewEncounterCard _ card -> do
      pure $ a & cardsUnderneathL %~ filter (/= toCard card)
    PlaceAsset aid placement | aid == assetId -> do
      let entersPlay = not (isInPlayPlacement a.placement) && isInPlayPlacement placement
      modifiers <- getCombinedModifiers [toTarget a, CardIdTarget (toCardId a)]
      let currentUses = Map.filterWithKey (\k _ -> tokenIsUse k) assetTokens
      startingUses <- toModifiedStartingUses a assetPrintedUses
      let uses = if currentUses == mempty && entersPlay then startingUses else mempty

      -- If the card wasn't in play, but moves into a play area we need to
      -- update the controller
      --
      -- See: The Beyond: Bleak Netherworld
      let
        mController = case placement of
          InPlayArea iid -> Just iid
          AttachedToAsset _ (Just (InPlayArea iid)) -> Just iid
          _ -> Nothing
        controllerF = case mController of
          Just iid | entersPlay -> controllerL ?~ iid
          _ -> id
      -- we should update control here if need be
      for_ placement.attachedTo \target ->
        pushM $ checkAfter $ Window.AttachCard a.controller (toCard a) target
      checkEntersThreatArea a placement

      when entersPlay do
        whenEnterMsg <- checkWindows [mkWhen (Window.EnterPlay $ toTarget a)]
        afterEnterMsg <- checkWindows [mkAfter (Window.EnterPlay $ toTarget a)]
        let startingDoom = sum [n | EntersPlayWithDoom n <- modifiers]

        let
          mEnterPlayMsg = case placement of
            InPlayArea iid -> Just $ CardEnteredPlay iid (toCard a)
            AttachedToAsset _ (Just (InPlayArea iid)) -> Just $ CardEnteredPlay iid (toCard a)
            _ -> Nothing

        pushAll
          $ [ActionCannotBeUndone | not assetCanLeavePlayByNormalMeans]
          <> [whenEnterMsg]
          <> maybeToList mEnterPlayMsg
          <> [PlaceDoom GameSource (toTarget a) startingDoom | startingDoom > 0]
          <> [afterEnterMsg]
      pure $ a & placementL .~ placement & controllerF & (tokensL %~ Map.unionWith (+) uses . coerce)
    Blanked msg' -> runMessage msg' a
    RemoveAllAttachments source target -> do
      case placementToAttached a.placement of
        Just attached | target == attached -> push $ toDiscard source a
        _ -> pure ()
      pure a
    _ -> pure a
