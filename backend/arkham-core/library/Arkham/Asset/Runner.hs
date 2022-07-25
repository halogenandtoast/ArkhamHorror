{-# OPTIONS_GHC -Wno-orphans #-}
module Arkham.Asset.Runner
  ( module X
  ) where

import Arkham.Prelude

import Arkham.Asset.Attrs as X
import Arkham.Asset.Helpers as X
import Arkham.Asset.Uses as X
import Arkham.Classes as X
import Arkham.Message as X hiding ( AssetDamage )

import Arkham.Card
import Arkham.DefeatedBy
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Message qualified as Msg
import Arkham.Placement
import Arkham.Projection
import Arkham.Source
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Window ( Window (..) )
import Arkham.Window qualified as Window

defeated :: (Monad m, HasGame m) => AssetAttrs -> m (Maybe DefeatedBy)
defeated AssetAttrs {assetId} = do
  remainingHealth <- field AssetRemainingHealth assetId
  remainingSanity <- field AssetRemainingSanity assetId
  pure $ case (remainingHealth, remainingSanity) of
    (Just a, Just b) | a <= 0 && b <= 0 -> Just DefeatedByDamageAndHorror
    (Just a, _) | a <= 0 -> Just DefeatedByDamage
    (_, Just b) | b <= 0 -> Just DefeatedByHorror
    _ -> Nothing

instance RunMessage AssetAttrs where
  runMessage msg a@AssetAttrs {..} = case msg of
    SetOriginalCardCode cardCode -> pure $ a & originalCardCodeL .~ cardCode
    SealedToken token card | toCardId card == toCardId a ->
      pure $ a & sealedTokensL %~ (token :)
    UnsealToken token -> pure $ a & sealedTokensL %~ filter (/= token)
    ReadyExhausted -> case assetPlacement of
      InPlayArea iid -> do
        modifiers <- getModifiers (toSource a) (InvestigatorTarget iid)
        if ControlledAssetsCannotReady `elem` modifiers
          then pure a
          else a <$ push (Ready $ toTarget a)
      _ -> a <$ push (Ready $ toTarget a)
    RemoveAllDoom _ -> pure $ a & doomL .~ 0
    PlaceClues target n | isTarget a target -> pure $ a & cluesL +~ n
    PlaceDoom target n | isTarget a target -> pure $ a & doomL +~ n
    RemoveDoom target n | isTarget a target ->
      pure $ a & doomL %~ min 0 . subtract n
    RemoveClues target n | isTarget a target -> do
      when (assetClues - n <= 0) $ pushAll =<< windows
        [Window.LastClueRemovedFromAsset (toId a)]
      pure $ a & cluesL %~ max 0 . subtract n
    CheckDefeated _ -> do
      mDefeated <- defeated a
      for_ mDefeated $ \defeatedBy -> do
          whenWindow <- checkWindows
            [Window Timing.When (Window.AssetDefeated (toId a) defeatedBy)]
          afterWindow <- checkWindows
            [Window Timing.When (Window.AssetDefeated (toId a) defeatedBy)]
          pushAll
            $ [whenWindow]
            <> resolve (AssetDefeated assetId)
            <> [afterWindow]
      pure a
    AssetDefeated aid | aid == assetId -> a <$ push (Discard $ toTarget a)
    Msg.AssetDamage aid _ damage horror | aid == assetId -> do
      pushAll $
        [PlaceDamage (toTarget a) damage | damage > 0]
        <> [PlaceHorror (toTarget a) horror | horror > 0]
      pure a
    PlaceDamage target n | isTarget a target ->
      pure $ a & damageL +~ n
    PlaceHorror target n | isTarget a target ->
      pure $ a & horrorL +~ n
    HealDamage (isTarget a -> True) n ->
      pure $ a & damageL %~ max 0 . subtract n
    HealHorror (isTarget a -> True) n ->
      pure $ a & horrorL %~ max 0 . subtract n
    When (InvestigatorResigned iid) -> do
      let
        shouldResignWith = case assetPlacement of
          InPlayArea iid' -> iid == iid'
          InThreatArea iid' -> iid == iid'
          AttachedToInvestigator iid' -> iid == iid'
          _ -> False
      when shouldResignWith $
        push $ ResignWith (AssetTarget assetId)
      pure a
    InvestigatorEliminated iid -> do
      let
        shouldDiscard = case assetPlacement of
          InPlayArea iid' -> iid == iid'
          InThreatArea iid' -> iid == iid'
          AttachedToInvestigator iid' -> iid == iid'
          _ -> False
      when shouldDiscard $
        push $ Discard (AssetTarget assetId)
      pure a
    AddUses target useType' n | a `isTarget` target -> case assetUses of
      Uses useType'' m | useType' == useType'' ->
        pure $ a & usesL .~ Uses useType' (n + m)
      _ -> error "Trying to add the wrong use type"
    SpendUses target useType' n | isTarget a target -> case assetUses of
      Uses useType'' m | useType' == useType'' -> do
        let remainingUses = max 0 (m - n)
        when
          (assetDiscardWhenNoUses && remainingUses == 0)
          (push $ Discard $ toTarget a)
        pure $ a & usesL .~ Uses useType' remainingUses
      _ -> error "Trying to use the wrong use type"
    AttachAsset aid target | aid == assetId -> case target of
      LocationTarget lid ->
        pure
          $ a
          & (placementL .~ AttachedToLocation lid)
      EnemyTarget eid ->
        pure
          $ a
          & (placementL .~ AttachedToEnemy eid)
      _ -> error "Cannot attach asset to that type"
    RemoveFromGame target | a `isTarget` target ->
      a <$ push (RemoveFromPlay $ toSource a)
    Discard target | a `isTarget` target -> do
      windows' <- windows [Window.WouldBeDiscarded (toTarget a)]
      a <$ pushAll
        (windows'
        <> [RemoveFromPlay $ toSource a, Discarded (toTarget a) (toCard a)]
        )
    Exile target | a `isTarget` target ->
      a <$ pushAll [RemoveFromPlay $ toSource a, Exiled target (toCard a)]
    RemoveFromPlay source | isSource a source -> do
      windowMsg <- checkWindows
        ((`Window` Window.LeavePlay (toTarget a))
        <$> [Timing.When, Timing.After]
        )
      pushAll
        $ windowMsg
        : [ UnsealToken token | token <- assetSealedTokens ]
        <> [RemovedFromPlay source]
      pure a
    InvestigatorPlayedAsset iid aid | aid == assetId -> do
      -- we specifically use the investigator source here because the
      -- asset has no knowledge of being owned yet, and this will allow
      -- us to bring the investigator's id into scope
      modifiers <- getModifiers (InvestigatorSource iid) (toTarget a)
      let
        startingUses = cdUses $ toCardDef a
        applyModifier (Uses uType m) (AdditionalStartingUses n) =
          Uses uType (n + m)
        applyModifier m _ = m
      whenEnterMsg <- checkWindows
        [Window Timing.When (Window.EnterPlay $ toTarget a)]
      afterEnterMsg <- checkWindows
        [Window Timing.After (Window.EnterPlay $ toTarget a)]

      pushAll
        $ [ ActionCannotBeUndone | not assetCanLeavePlayByNormalMeans ]
        <> [whenEnterMsg, afterEnterMsg]
      pure
        $ a
        & (placementL .~ InPlayArea iid)
        & (usesL .~ if assetUses == NoUses
            then foldl' applyModifier startingUses modifiers
            else assetUses
          )
    TakeControlOfAsset iid aid | aid == assetId -> do
      push =<< checkWindows
        ((`Window` Window.TookControlOfAsset iid aid)
        <$> [Timing.When, Timing.After]
        )
      pure $ a & placementL .~ InPlayArea iid
    ReplacedInvestigatorAsset iid aid | aid == assetId ->
      pure $ a & placementL .~ InPlayArea iid
    AddToScenarioDeck key target | isTarget a target -> do
      pushAll
        [AddCardToScenarioDeck key (toCard a), RemoveFromGame (toTarget a)]
      pure $ a & placementL .~ Unplaced
    Exhaust target | a `isTarget` target -> pure $ a & exhaustedL .~ True
    Ready target | a `isTarget` target -> case assetPlacement of
      InPlayArea iid -> do
        modifiers <- getModifiers (toSource a) (InvestigatorTarget iid)
        if ControlledAssetsCannotReady `elem` modifiers
          then pure a
          else pure $ a & exhaustedL .~ False
      _ -> pure $ a & exhaustedL .~ False
    PlaceUnderneath (isTarget a -> True) cards -> do
      pure $ a & cardsUnderneathL <>~ cards
    PlaceAsset aid placement | aid == assetId ->
      pure $ a & placementL .~ placement
    Blanked msg' -> runMessage msg' a
    _ -> pure a
