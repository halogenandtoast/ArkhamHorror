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
import Arkham.Helpers.Message as X hiding (AssetDamage, RevealChaosToken)
import Arkham.Helpers.SkillTest as X
import Arkham.SkillTest.Base as X (SkillTestDifficulty (..))
import Arkham.Source as X
import Arkham.Target as X

import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes.HasGame
import Arkham.Damage
import Arkham.DefeatedBy
import Arkham.Event.Types (Field (EventUses))
import Arkham.Helpers.Calculation (calculate)
import Arkham.Helpers.Customization
import Arkham.Helpers.Placement
import Arkham.Helpers.Use
import Arkham.Matcher (
  AssetMatcher (AnyAsset, AssetAttachedToAsset, AssetWithId),
 )
import Arkham.Message qualified as Msg
import Arkham.Projection
import Arkham.Timing qualified as Timing
import Arkham.Token qualified as Token
import Arkham.Window (mkAfter, mkWindow)
import Arkham.Window qualified as Window
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
    IncreaseCustomization iid cardCode customization choices | toCardCode a == cardCode && a `ownedBy` iid -> do
      case customizationIndex a customization of
        Nothing -> pure a
        Just i ->
          pure
            $ a {assetCustomizations = IntMap.adjust (second (const choices) . first (+ 1)) i assetCustomizations}
    SetOriginalCardCode cardCode -> pure $ a & originalCardCodeL .~ cardCode
    SealedChaosToken token card | toCardId card == toCardId a -> do
      pure $ a & sealedChaosTokensL %~ (token :)
    UnsealChaosToken token -> pure $ a & sealedChaosTokensL %~ filter (/= token)
    ReturnChaosTokensToPool tokens -> pure $ a & sealedChaosTokensL %~ filter (`notElem` tokens)
    RemoveAllChaosTokens face -> do
      pure $ a & sealedChaosTokensL %~ filter ((/= face) . chaosTokenFace)
    ReadyExhausted -> case assetPlacement of
      InPlayArea iid -> do
        modifiers <- getModifiers (InvestigatorTarget iid)
        if ControlledAssetsCannotReady `elem` modifiers
          then pure a
          else a <$ push (Ready $ toTarget a)
      _ -> a <$ push (Ready $ toTarget a)
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
        else pure $ a & tokensL %~ addTokens tType n
    MoveTokens s source _ tType n | isSource a source -> do
      runMessage (RemoveTokens s (toTarget a) tType n) a
    MoveTokens s _ target tType n | isTarget a target -> do
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
          pushWhen (tType `elem` [Horror, Damage]) $ checkDefeated s a
          pure $ a & tokensL %~ addTokens tType n
    ClearTokens target | isTarget a target -> do
      when (assetClues a > 0)
        $ pushAll
        =<< windows
          [Window.LastClueRemovedFromAsset (toId a)]
      for_ assetWhenNoUses \case
        DiscardWhenNoUses -> push $ Discard assetController GameSource (toTarget a)
        ReturnToHandWhenNoUses ->
          for_ assetController \iid ->
            push $ ReturnToHand iid $ toTarget a
        NotifySelfOfNoUses -> push $ SpentAllUses (toTarget a)
      pure $ a & tokensL .~ mempty
    RemoveTokens _ target tType n | isTarget a target -> do
      when (tType == Clue && assetClues a - n <= 0)
        $ pushAll
        =<< windows
          [Window.LastClueRemovedFromAsset (toId a)]
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
        (before, _, after) <- frame (Window.AssetDefeated (toId a) defeatedBy)
        pushAll $ [before] <> resolve (AssetDefeated assetId) <> [after]
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
    AssetDefeated aid | aid == assetId -> do
      push $ toDiscard GameSource a
      pure a
    Msg.AssetDamageWithCheck aid source damage horror doCheck | aid == assetId -> do
      mods <- getModifiers a
      let n = sum [x | DamageTaken x <- mods]
      pushAll
        $ [PlaceDamage source (toTarget a) (damage + n) | damage > 0]
        <> [PlaceHorror source (toTarget a) horror | horror > 0]
        <> [checkDefeated source aid | doCheck]
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
      pushWhen shouldDiscard $ toDiscard GameSource assetId
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
          AbilitySource s i -> insertAfterMatching [afterLast] \case
            ResolvedAbility ab -> ab.source == s && ab.index == i
            _ -> False
          CardSource c -> insertAfterMatching [afterLast] \case
            ResolvedPlayCard _ c' -> c.id == c'.id
            _ -> False
          EventSource e -> do
            mAbility <- findFromQueue \case
              ResolvedAbility _ -> True
              _ -> False
            case mAbility of
              Nothing -> insertAfterMatching [afterLast] \case
                FinishedEvent e' -> e == e'
                _ -> False
              Just msg' -> insertAfterMatching [afterLast] (== msg')
          _ -> pure ()
      runQueueT do
        (before, _, after) <- frame $ Window.SpentToken source (toTarget a) useType' n
        -- window should be independent of other sources since they are spent from this asset
        push before
        for_ assetController $ \controller ->
          when (tokenIsUse useType') do
            (before1, _, after1) <- frame $ Window.SpentUses controller source (toId a) useType' n
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
      windows' <- windows [Window.WouldBeDiscarded (toTarget a)]
      afterWindows <- checkAfter $ Window.Discarded mInvestigator source (toCard a)
      let discardMsg = if removeFromGame then RemoveFromGame (toTarget a) else Discarded (toTarget a) source (toCard a)
      pushAll
        $ windows'
        <> [RemoveFromPlay $ toSource a, discardMsg, afterWindows]
      pure a
    Exile target | a `isTarget` target -> do
      pushAll [RemoveFromPlay $ toSource a, Exiled target (toCard a)]
      pure $ a & exiledL .~ True
    RemoveFromPlay source | isSource a source -> do
      attachedAssets <- select $ AssetAttachedToAsset $ AssetWithId (toId a)
      windowMsg <-
        checkWindows
          ( (`mkWindow` Window.LeavePlay (toTarget a))
              <$> [Timing.When, Timing.AtIf, Timing.After]
          )
      pushAll
        $ windowMsg
        : [UnsealChaosToken token | token <- assetSealedChaosTokens]
          <> [Discard Nothing GameSource (toTarget a') | a' <- attachedAssets]
          <> [RemovedFromPlay source]
      pure a
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
      pure $ a & tokensL %~ removeAllTokens Token.Horror
    InvestigatorPlayedAsset iid aid | aid == assetId -> do
      -- we specifically use the investigator source here because the
      -- asset has no knowledge of being owned yet, and this will allow
      -- us to bring the investigator's id into scope
      modifiers <- getCombinedModifiers [toTarget a, CardIdTarget (toCardId a)]
      let printedUses = cdUses (toCardDef a)
      startingUses <- toModifiedStartingUses a printedUses
      let startingDoom = sum [n | EntersPlayWithDoom n <- modifiers]
      whenEnterMsg <-
        checkWindows
          [mkWindow Timing.When (Window.EnterPlay $ toTarget a)]
      afterEnterMsg <-
        checkWindows
          [mkWindow Timing.After (Window.EnterPlay $ toTarget a)]

      pushAll
        $ [ActionCannotBeUndone | not assetCanLeavePlayByNormalMeans]
        <> [whenEnterMsg]
        <> [PlaceDoom GameSource (toTarget a) startingDoom | startingDoom > 0]
        <> [afterEnterMsg]

      let placementF = case assetPlacement of
            Unplaced -> placementL .~ InPlayArea iid
            _ -> id
          controllerF = case assetController of
            Nothing -> controllerL ?~ iid
            Just _ -> id
          currentUses = Map.filterWithKey (\k _ -> tokenIsUse k) assetTokens

      let uses = if currentUses == mempty then startingUses else mempty

      pure
        $ a
        & placementF
        & controllerF
        & (tokensL %~ Map.unionWith (+) uses . coerce)
    TakeControlOfAsset iid aid | aid == assetId -> do
      push
        =<< checkWindows
          ( (`mkWindow` Window.TookControlOfAsset iid aid)
              <$> [Timing.When, Timing.After]
          )
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
    Exhaust target | a `isTarget` target -> do
      msgs <- doFrame (Exhaust target) (Window.Exhausts (toTarget a))
      pushAll msgs
      pure a
    Do (Exhaust target) | a `isTarget` target -> do
      pure $ a & exhaustedL .~ True
    ExhaustThen target msgs | a `isTarget` target -> do
      unless assetExhausted $ pushAll msgs
      pure $ a & exhaustedL .~ True
    Ready target | a `isTarget` target -> case assetPlacement of
      InPlayArea iid -> do
        modifiers <- getModifiers (InvestigatorTarget iid)
        if ControlledAssetsCannotReady `elem` modifiers
          then pure a
          else pure $ a & exhaustedL .~ False
      _ -> pure $ a & exhaustedL .~ False
    PlaceUnderneath (isTarget a -> True) cards -> do
      pure $ a & cardsUnderneathL <>~ cards
    AddToDiscard _ c -> do
      pure $ a & cardsUnderneathL %~ filter (/= toCard c)
    AddToEncounterDiscard c -> do
      pure $ a & cardsUnderneathL %~ filter (/= toCard c)
    CommitCard _ card -> do
      pure $ a & cardsUnderneathL %~ filter (/= card)
    AddToHand _ cards -> do
      pure $ a & cardsUnderneathL %~ filter (`notElem` cards)
    InvestigatorDrewPlayerCard _ card -> do
      pure $ a & cardsUnderneathL %~ filter (/= toCard card)
    InvestigatorDrewEncounterCard _ card -> do
      pure $ a & cardsUnderneathL %~ filter (/= toCard card)
    PlaceAsset aid placement | aid == assetId -> do
      -- we should update control here if need be
      for_ placement.attachedTo \target ->
        pushM $ checkAfter $ Window.AttachCard a.controller (toCard a) target
      checkEntersThreatArea a placement
      pure $ a & placementL .~ placement
    Blanked msg' -> runMessage msg' a
    RemoveAllAttachments source target -> do
      case placementToAttached a.placement of
        Just attached | target == attached -> push $ toDiscard source a
        _ -> pure ()
      pure a
    _ -> pure a
