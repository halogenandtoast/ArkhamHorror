{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Skill.Runner (module X) where

import Arkham.Helpers.Effect as X
import Arkham.Helpers.Message as X hiding (
  InvestigatorDamage,
  InvestigatorEliminated,
  PlayCard,
  RevealLocation,
 )
import Arkham.Helpers.Query as X
import Arkham.Id as X
import Arkham.Skill.Types as X
import Arkham.Source as X
import Arkham.Target as X

import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes.Entity
import Arkham.Classes.RunMessage
import Arkham.Helpers.Customization
import Arkham.Helpers.Window (checkAfter, checkWhen, checkWindows, wouldWindows)
import Arkham.Placement
import Arkham.Prelude
import Arkham.Window (mkWindow)
import Arkham.Window qualified as Window
import Data.IntMap.Strict qualified as IntMap

instance RunMessage SkillAttrs where
  runMessage msg a = case msg of
    IncreaseCustomization iid cardCode customization choices | toCardCode a == cardCode && a.owner == iid -> do
      case customizationIndex a customization of
        Nothing -> pure a
        Just i ->
          pure
            $ a
              { skillCustomizations =
                  IntMap.alter (Just . maybe (1, choices) (bimap (+ 1) (const choices))) i a.customizations
              }
    SealedChaosToken token _ (isTarget a -> True) -> do
      pure $ a & sealedChaosTokensL %~ addSealedChaosToken token
    SealedChaosToken token _ _ -> do
      pure $ a & sealedChaosTokensL %~ filter (/= token)
    UnsealChaosToken token -> do
      when (token `elem` skillSealedChaosTokens a) do
        let released = Window.ChaosTokenReleased (toTarget a) token
        pushAllM $ sequence [checkWhen released, checkAfter released]
      pure $ a & sealedChaosTokensL %~ filter (/= token)
    RemoveAllChaosTokens face -> do
      pure $ a & sealedChaosTokensL %~ filter ((/= face) . chaosTokenFace)
    UseCardAbility _ (isSource a -> True) (-1) _ payment -> do
      pure $ a {skillAdditionalPayment = Just payment}
    InvestigatorCommittedSkill _ skillId | skillId == toId a -> do
      pure $ a {skillPlacement = Limbo}
    PlaceSkill sid placement | sid == toId a -> do
      for_ placement.attachedTo \target ->
        pushM $ checkAfter $ Window.AttachCard (Just a.controller) (toCard a) target
      pure $ a {skillPlacement = placement}
    RemoveAllAttachments source target -> do
      case placementToAttached a.placement of
        Just attached | target == attached -> push $ toDiscard source a
        _ -> pure ()
      pure a
    {- Nothing discarded a skill ENTITY. A committed skill is swept up when the
    test ends, and until a card attached one to a location there was nowhere
    else for one to be -- so `Discard` on a `SkillTarget` reached no handler at
    all and the card stayed where it was. Modelled on the event's: the
    would-window, then the card to its owner's discard pile, with `Discarded`
    taking it out of play. -}
    Discard _ source (isTarget a -> True) -> do
      (batchId, windowMessages) <- wouldWindows $ Window.WouldBeDiscarded (toTarget a)
      let card = toCard a
      let owner = fromMaybe a.owner (toCardOwner card)
      let toPile = case card of
            PlayerCard pc -> [AddToDiscard owner pc]
            _ -> []
      push $ Would batchId $ windowMessages <> [Discarded (toTarget a) source card] <> toPile
      pure a
    Discarded (isTarget a -> True) _ _ -> do
      runMessage (RemoveFromPlay (toSource a)) a
    RemoveFromPlay source | isSource a source -> do
      windowMsg <-
        checkWindows
          ( (`mkWindow` Window.LeavePlay (toTarget a))
              <$> [#when, #at, #after]
          )
      pushAll
        $ windowMsg
        : [UnsealChaosToken token | token <- skillSealedChaosTokens a]
          <> [RemovedFromPlay source]
      pure a
    UseAbility _ ab _ | isSource a ab.source || isProxySource a ab.source -> do
      push $ Do msg
      pure a
    InSearch msg'@(UseAbility _ ab _) | isSource a ab.source || isProxySource a ab.source -> do
      push $ Do msg'
      pure a
    -- NOTE: No InDiscard UseAbility handler here. In-discard entities also receive the raw
    -- message (see RunMessage Game in Game/Runner.hs), so the bare handler above already
    -- fires for them; a second one pushes `Do (UseAbility)` twice and resolves the ability
    -- (and pays its cost) twice -- see #4764 and Monastic Training's doubled reaction.
    InHand iid msg'@(UseAbility iid' ab _) | iid == iid' && (isSource a ab.source || isProxySource a ab.source) -> do
      push $ Do msg'
      pure a
    Committed iid msg'@(UseAbility iid' ab _) | iid == iid' && (isSource a ab.source || isProxySource a ab.source) -> do
      push $ Do msg'
      pure a
    SetLocationOutOfGame lid -> do
      case skillPlacement a of
        p@(AtLocation lid') | lid' == lid -> pure $ a & placementL .~ OutOfGame p
        p@(AttachedToLocation lid') | lid' == lid -> pure $ a & placementL .~ OutOfGame p
        _ -> pure a
    ReturnLocationToGame lid -> do
      case skillPlacement a of
        OutOfGame p@(AtLocation lid') | lid' == lid -> pure $ a & placementL .~ p
        OutOfGame p@(AttachedToLocation lid') | lid' == lid -> pure $ a & placementL .~ p
        _ -> pure a
    _ -> pure a
