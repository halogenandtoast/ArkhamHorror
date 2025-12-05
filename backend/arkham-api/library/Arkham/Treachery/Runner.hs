{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Treachery.Runner (module X, addHiddenToHand, forcedOnElimination, on) where

import Arkham.Calculation as X
import Arkham.Classes.Entity as X
import Arkham.Classes.HasAbilities as X
import Arkham.Classes.HasModifiersFor as X
import Arkham.Classes.Query as X
import Arkham.Classes.RunMessage as X
import Arkham.Helpers.Effect as X
import Arkham.Helpers.Investigator as X (eliminationWindow)
import Arkham.Helpers.Message as X hiding (
  DeckHasNoCards,
  EnemyDefeated,
  InvestigatorDamage,
  InvestigatorEliminated,
  RevealChaosToken,
  is,
  toDiscard,
  toDiscardBy,
  addToVictory,
 )
import Arkham.Helpers.Query as X
import Arkham.Helpers.SkillTest as X
import Arkham.Id as X
import Arkham.Placement as X
import Arkham.SkillTest.Base as X (SkillTestDifficulty (..))
import Arkham.Source as X
import Arkham.Target as X
import Arkham.Treachery.Helpers as X
import Arkham.Treachery.Types as X

import Arkham.Ability.Type
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Helpers.Card (getHasVictoryPoints)
import Arkham.Helpers.Modifiers (getModifiers)
import Arkham.Helpers.Ref (sourceToTarget)
import Arkham.Matcher.Base (Be (..))
import Arkham.Matcher.Treachery (TreacheryMatcher (TreacheryWithId))
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted
import Arkham.Modifier (ModifierType (RevelationModifier))
import Arkham.Prelude
import Arkham.Token
import Arkham.Window (mkWindow)
import Arkham.Window qualified as Window

addHiddenToHand :: InvestigatorId -> TreacheryAttrs -> Message
addHiddenToHand iid a = PlaceTreachery (toId a) (HiddenInHand iid)

forcedOnElimination :: InvestigatorId -> AbilityType
forcedOnElimination = ForcedAbility . eliminationWindow

on :: Targetable target => TreacheryAttrs -> target -> Bool
on = treacheryOn

instance Be TreacheryAttrs TreacheryMatcher where
  be = TreacheryWithId . toId

instance RunMessage TreacheryAttrs where
  runMessage msg a@TreacheryAttrs {..} = runQueueT $ case msg of
    Msg.SealedChaosToken token _ (isTarget a -> True) -> do
      pure $ a & sealedChaosTokensL %~ (token :)
    Msg.UnsealChaosToken token -> pure $ a & sealedChaosTokensL %~ filter (/= token)
    Msg.RemoveAllChaosTokens face -> do
      pure $ a & sealedChaosTokensL %~ filter ((/= face) . chaosTokenFace)
    Msg.InvestigatorEliminated iid -> do
      let owned = Just iid == treacheryOwner
      let
        shouldDiscard = case a.placement of
          InThreatArea iid' -> iid == iid'
          InPlayArea iid' -> iid == iid'
          HiddenInHand iid' -> iid == iid'
          AttachedToInvestigator iid' -> iid == iid'
          _ -> False

      when (shouldDiscard || owned) $ toDiscard GameSource a
      pure a
    PlaceTreachery tid placement | tid == treacheryId -> do
      for_ placement.attachedTo $ checkWhen . Window.AttachCard Nothing (toCard a)
      let entersPlay = not (isInPlayPlacement a.placement) && isInPlayPlacement placement
      case placement of
        HiddenInHand _ -> obtainCard a
        InThreatArea iid -> do
          checkWindows $ Window.mkAfter (Window.EntersThreatArea iid $ toCard a)
            : [Window.mkAfter $ Window.TreacheryEntersPlay tid | entersPlay]
        _ -> when entersPlay do
          checkAfter $ Window.TreacheryEntersPlay tid
      for_ placement.attachedTo $ checkAfter . Window.AttachCard Nothing (toCard a)
      pure $ a & placementL .~ placement
    PlaceTokens source target tType n | isTarget a target -> runQueueT do
      push $ Do msg
      checkWhen $ Window.PlacedToken source target tType n
      pure a
    Do (PlaceTokens source target@(isTarget a -> True) token n) -> do
      when (token == Doom && a.doom == 0) do
        checkAfter $ Window.PlacedDoomCounterOnTargetWithNoDoom source (toTarget a) n
      checkAfter $ Window.PlacedToken source target token n
      pure $ a & tokensL %~ addTokens token n
    RemoveTokens _ (isTarget a -> True) token n -> pure $ a & tokensL %~ subtractTokens token n
    MoveTokens s source _ tType n | isSource a source -> liftRunMessage (RemoveTokens s (toTarget a) tType n) a
    MoveTokens _s (InvestigatorSource _) target Clue _ | isTarget a target -> pure a
    MoveTokens s _ target tType n | isTarget a target -> liftRunMessage (PlaceTokens s (toTarget a) tType n) a
    RemoveAllDoom _ (isTarget a -> True) -> do
      pure $ a & tokensL %~ removeAllTokens Doom
    PlaceEnemyOutOfPlay _ eid | EnemyTarget eid `elem` treacheryAttachedTarget a -> do
      toDiscard GameSource a
      pure a
    Discard miid _ (TreacheryTarget tid) | tid == treacheryId -> do
      pure $ a & discardedByL .~ miid
    After (RemoveTreachery tid) | tid == treacheryId -> do
      pure $ a & placementL .~ Unplaced
    PutOnBottomOfDeck iid deck target | a `isTarget` target -> do
      pushAll [RemoveTreachery $ toId a, PutCardOnBottomOfDeck iid deck (toCard a)]
      pure a
    AddToVictory miid target | target `elem` treacheryAttachedTarget a -> do
      case miid of
        Nothing -> toDiscard GameSource a
        Just iid -> toDiscardBy iid GameSource a
      pure a
    When (Revelation iid (isSource a -> True)) -> do
      mods <- getModifiers (toCardId a)
      for_ mods \case
        RevelationModifier source inner ->
          eachInvestigator \iid' -> revelationModifier source iid' a.id inner
        _ -> pure ()
      checkWhen $ Window.ResolvingRevelation iid a.id
      pure a
    After (Revelation iid (isSource a -> True)) -> do
      when (treacheryPlacement == Limbo) do
        hasVictory <- getHasVictoryPoints (toCard a)
        if hasVictory
          then addToVictory iid a
          else toDiscardBy iid GameSource a
      pure $ a & resolvedL %~ insertSet iid
    RemoveAllAttachments source target -> do
      case a.placement.attachedTo of
        Just attached | target == attached -> toDiscard source a
        _ -> pure ()
      pure a
    RemoveAllCopiesOfCardFromGame _ cCode | cCode == toCardCode a -> do
      push $ RemoveTreachery (toId a)
      pure a
    RemoveAllCopiesOfEncounterCardFromGame cardMatcher | toCard a `cardMatch` cardMatcher -> do
      push $ RemoveTreachery (toId a)
      pure a
    ReplaceAct aid _ -> do
      when (treacheryOnAct aid a) $ toDiscard (toSource a) (toTarget a)
      pure a
    ReplaceAgenda aid _ -> do
      when (treacheryOnAgenda aid a) $ toDiscard (toSource a) (toTarget a)
      pure a
    Discarded (isTarget a -> True) _ _ -> do
      liftRunMessage (RemoveFromPlay (toSource a)) a
    RemoveFromGame target | a `isTarget` target -> do
      a <$ push (RemoveFromPlay $ toSource a)
    RemoveFromPlay source | isSource a source -> do
      checkWindows
        $ (`mkWindow` Window.LeavePlay (toTarget a))
        <$> [#when, #at, #after]
      pushAll
        $ [UnsealChaosToken token | token <- treacherySealedChaosTokens]
        <> [RemovedFromPlay source]
      pure a
    RemovedFromPlay source -> do
      case a.attached of
        Just target | isTarget target (sourceToTarget source) -> toDiscard GameSource (toTarget a)
        _ -> pure ()
      pure a
    Exhaust (isTarget a -> True) -> do
      pure $ a & exhaustedL .~ True
    ReadyExhausted -> do
      push $ Ready $ toTarget a
      pure a
    Ready (isTarget a -> True) -> do
      pure $ a & exhaustedL .~ False
    Do (AfterRevelation _ tid) | tid == treacheryId -> do
      pure $ a & waitingL .~ False
    UseAbility _ ab _ | isSource a ab.source || isProxySource a ab.source -> do
      push $ Do msg
      pure a
    InSearch msg'@(UseAbility _ ab _) | isSource a ab.source || isProxySource a ab.source -> do
      push $ Do msg'
      pure a
    InDiscard iid msg'@(UseAbility iid' ab _) | iid == iid' && (isSource a ab.source || isProxySource a ab.source) -> do
      push $ Do msg'
      pure a
    InHand iid msg'@(UseAbility iid' ab _) | iid == iid' && (isSource a ab.source || isProxySource a ab.source) -> do
      push $ Do msg'
      pure a
    SetLocationOutOfGame lid -> do
      case treacheryPlacement of
        AtLocation lid' | lid' == lid -> pure $ a & placementL .~ OutOfGame treacheryPlacement
        AttachedToLocation lid' | lid' == lid -> pure $ a & placementL .~ OutOfGame treacheryPlacement
        _ -> pure a
    ReturnLocationToGame lid -> do
      case treacheryPlacement of
        OutOfGame p@(AtLocation lid') | lid' == lid -> pure $ a & placementL .~ p
        OutOfGame p@(AttachedToLocation lid') | lid' == lid -> pure $ a & placementL .~ p
        _ -> pure a
    _ -> pure a
