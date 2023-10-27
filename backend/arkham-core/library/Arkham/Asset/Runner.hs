{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Asset.Runner (
  module X,
  hasUses,
) where

import Arkham.Prelude

import Arkham.Asset.Helpers as X
import Arkham.Asset.Types as X
import Arkham.Asset.Uses as X
import Arkham.Classes as X
import Arkham.GameValue as X
import Arkham.Helpers.Message as X hiding (AssetDamage, RevealChaosToken)
import Arkham.Helpers.SkillTest as X
import Arkham.Source as X
import Arkham.Target as X

import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes.HasGame
import Arkham.Damage
import Arkham.DefeatedBy
import Arkham.Helpers.Use
import Arkham.Matcher (AssetMatcher (AnyAsset))
import Arkham.Message qualified as Msg
import Arkham.Placement
import Arkham.Projection
import Arkham.Timing qualified as Timing
import Arkham.Token
import Arkham.Token qualified as Token
import Arkham.Window (mkWindow)
import Arkham.Window qualified as Window

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
hasUses = (> 0) . useCount . assetUses

instance RunMessage Asset where
  runMessage msg x@(Asset a) = do
    inPlay <- member (toId x) <$> select AnyAsset
    modifiers' <- if inPlay then getModifiers (toTarget x) else pure []
    let msg' = if Blank `elem` modifiers' then Blanked msg else msg
    Asset <$> runMessage msg' a

instance RunMessage AssetAttrs where
  runMessage msg a@AssetAttrs {..} = case msg of
    SetOriginalCardCode cardCode -> pure $ a & originalCardCodeL .~ cardCode
    SealedChaosToken token card | toCardId card == toCardId a -> do
      pure $ a & sealedChaosTokensL %~ (token :)
    UnsealChaosToken token -> pure $ a & sealedChaosTokensL %~ filter (/= token)
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
    PlaceTokens _ target tType n | isTarget a target -> pure $ a & tokensL %~ addTokens tType n
    RemoveClues _ target n | isTarget a target -> do
      when (assetClues a - n <= 0)
        $ pushAll
        =<< windows
          [Window.LastClueRemovedFromAsset (toId a)]
      pure $ a & tokensL %~ subtractTokens Clue n
    RemoveTokens _ target tType n | isTarget a target -> do
      pure $ a & tokensL %~ subtractTokens tType n
    CheckDefeated source (isTarget a -> True) -> do
      mDefeated <- defeated a source
      for_ mDefeated $ \defeatedBy -> do
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
    AssetDefeated aid | aid == assetId -> a <$ push (Discard GameSource $ toTarget a)
    Msg.AssetDamageWithCheck aid source damage horror doCheck | aid == assetId -> do
      pushAll
        $ [PlaceDamage source (toTarget a) damage | damage > 0]
        <> [PlaceHorror source (toTarget a) horror | horror > 0]
        <> [checkDefeated source aid | doCheck]
      pure a
    MovedClues _ (isTarget a -> True) amount -> do
      pure $ a & tokensL %~ addTokens #clue amount
    MovedClues (isSource a -> True) _ amount -> do
      pure $ a & tokensL %~ subtractTokens #clue amount
    MovedHorror (isSource a -> True) _ n -> do
      pure $ a & tokensL %~ subtractTokens #horror n
    MovedHorror source (isTarget a -> True) n -> do
      push $ checkDefeated source a
      pure $ a & tokensL %~ addTokens #horror n
    MovedDamage source (isTarget a -> True) amount -> do
      push $ checkDefeated source a
      pure $ a & tokensL %~ addTokens #damage amount
    MovedDamage (isSource a -> True) _ amount -> do
      pure $ a & tokensL %~ subtractTokens #damage amount
    HealDamage (isTarget a -> True) source n -> do
      afterWindow <- checkWindows [mkWindow Timing.After (Window.Healed DamageType (toTarget a) source n)]
      push afterWindow
      pure $ a & tokensL %~ subtractTokens Token.Damage n
    HealHorror (isTarget a -> True) source n -> do
      afterWindow <- checkWindows [mkWindow Timing.After (Window.Healed HorrorType (toTarget a) source n)]
      push afterWindow
      pure $ a & tokensL %~ subtractTokens Horror n
    HealHorrorDirectly target _ amount | isTarget a target -> do
      -- USE ONLY WHEN NO CALLBACKS
      pure $ a & tokensL %~ subtractTokens Horror amount
    HealDamageDirectly target _ amount | isTarget a target -> do
      -- USE ONLY WHEN NO CALLBACKS
      pure $ a & tokensL %~ subtractTokens Token.Damage amount
    When (InvestigatorResigned iid) -> do
      let
        shouldResignWith = case assetPlacement of
          InPlayArea iid' -> iid == iid'
          InThreatArea iid' -> iid == iid'
          AttachedToInvestigator iid' -> iid == iid'
          _ -> False
      when shouldResignWith $ push $ ResignWith (AssetTarget assetId)
      pure a
    InvestigatorEliminated iid -> do
      let
        shouldDiscard = case assetPlacement of
          InPlayArea iid' -> iid == iid'
          InThreatArea iid' -> iid == iid'
          AttachedToInvestigator iid' -> iid == iid'
          _ -> False
      when shouldDiscard $ push $ Discard GameSource (AssetTarget assetId)
      pure a
    AddUses aid useType' n | aid == assetId -> case assetUses of
      Uses useType'' m | useType' == useType'' -> do
        pure $ a & usesL .~ Uses useType' (n + m)
      UsesWithLimit useType'' m l | useType' == useType'' -> do
        pure $ a & usesL .~ UsesWithLimit useType' (min (n + m) l) l
      _ ->
        error $ "Trying to add the wrong use type, has " <> show assetUses <> ", but got: " <> show useType'
    SpendUses target useType' n | isTarget a target -> case assetUses of
      Uses useType'' m | useType' == useType'' -> do
        let remainingUses = max 0 (m - n)
        when (remainingUses == 0)
          $ for_ assetWhenNoUses \case
            DiscardWhenNoUses -> push $ Discard GameSource $ toTarget a
            ReturnToHandWhenNoUses ->
              for_ assetController $ \iid ->
                push $ ReturnToHand iid $ toTarget a
            NotifySelfOfNoUses -> push $ SpentAllUses (toTarget a)
        pure $ a & usesL .~ Uses useType' remainingUses
      _ -> error "Trying to use the wrong use type"
    AttachAsset aid target | aid == assetId -> case target of
      LocationTarget lid -> pure $ a & (placementL .~ AttachedToLocation lid)
      EnemyTarget eid -> pure $ a & (placementL .~ AttachedToEnemy eid)
      _ -> error "Cannot attach asset to that type"
    RemoveFromGame target | a `isTarget` target -> do
      a <$ push (RemoveFromPlay $ toSource a)
    Discard source target | a `isTarget` target -> do
      windows' <- windows [Window.WouldBeDiscarded (toTarget a)]
      pushAll $ windows' <> [RemoveFromPlay $ toSource a, Discarded (toTarget a) source (toCard a)]
      pure a
    Exile target | a `isTarget` target -> do
      a <$ pushAll [RemoveFromPlay $ toSource a, Exiled target (toCard a)]
    RemoveFromPlay source | isSource a source -> do
      windowMsg <-
        checkWindows
          ( (`mkWindow` Window.LeavePlay (toTarget a))
              <$> [Timing.When, Timing.AtIf, Timing.After]
          )
      pushAll
        $ windowMsg
        : [UnsealChaosToken token | token <- assetSealedChaosTokens]
          <> [RemovedFromPlay source]
      pure a
    PlaceKey (isTarget a -> True) k -> do
      pure $ a & (keysL %~ insertSet k)
    InvestigatorPlayedAsset iid aid | aid == assetId -> do
      -- we specifically use the investigator source here because the
      -- asset has no knowledge of being owned yet, and this will allow
      -- us to bring the investigator's id into scope
      modifiers <- getModifiers (toTarget a)
      startingUses <- toStartingUses $ cdUses $ toCardDef a
      let
        applyModifier (Uses uType m) (AdditionalStartingUses n) =
          Uses uType (n + m)
        applyModifier (UsesWithLimit uType m l) (AdditionalStartingUses n) =
          UsesWithLimit uType (min l (n + m)) l
        applyModifier m _ = m
      whenEnterMsg <-
        checkWindows
          [mkWindow Timing.When (Window.EnterPlay $ toTarget a)]
      afterEnterMsg <-
        checkWindows
          [mkWindow Timing.After (Window.EnterPlay $ toTarget a)]

      pushAll
        $ [ActionCannotBeUndone | not assetCanLeavePlayByNormalMeans]
        <> [whenEnterMsg, afterEnterMsg]

      let placementF = case assetPlacement of
            Unplaced -> placementL .~ InPlayArea iid
            _ -> id
          controllerF = case assetController of
            Nothing -> controllerL ?~ iid
            Just _ -> id

      pure
        $ a
        & placementF
        & controllerF
        & ( usesL
              .~ if assetUses == NoUses
                then foldl' applyModifier startingUses modifiers
                else assetUses
          )
    TakeControlOfAsset iid aid | aid == assetId -> do
      push
        =<< checkWindows
          ( (`mkWindow` Window.TookControlOfAsset iid aid)
              <$> [Timing.When, Timing.After]
          )
      pure $ a & placementL .~ InPlayArea iid & controllerL ?~ iid
    ReplacedInvestigatorAsset iid aid | aid == assetId -> do
      pure $ a & placementL .~ InPlayArea iid & controllerL ?~ iid
    AddToScenarioDeck key target | isTarget a target -> do
      pushAll
        [AddCardToScenarioDeck key (toCard a), RemoveFromGame (toTarget a)]
      pure $ a & placementL .~ Unplaced
    ShuffleCardsIntoDeck _ cards ->
      pure $ a & cardsUnderneathL %~ filter (`notElem` cards)
    Exhaust target | a `isTarget` target -> pure $ a & exhaustedL .~ True
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
    CommitCard _ card -> do
      pure $ a & cardsUnderneathL %~ filter (/= card)
    AddToHand _ cards -> do
      pure $ a & cardsUnderneathL %~ filter (`notElem` cards)
    PlaceAsset aid placement | aid == assetId -> do
      pure $ a & placementL .~ placement
    Blanked msg' -> runMessage msg' a
    _ -> pure a
