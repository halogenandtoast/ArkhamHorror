{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Treachery.Runner (
  module X,
  addHiddenToHand,
  forcedOnElimination,
  on,
) where

import Arkham.Prelude

import Arkham.Classes.Entity as X
import Arkham.Classes.HasAbilities as X
import Arkham.Classes.HasModifiersFor as X
import Arkham.Classes.Query as X
import Arkham.Classes.RunMessage as X
import Arkham.Helpers.Investigator as X (eliminationWindow)
import Arkham.Helpers.Message as X hiding (
  AssetDamage,
  DeckHasNoCards,
  EnemyDefeated,
  InvestigatorDamage,
  InvestigatorEliminated,
  RevealChaosToken,
  is,
 )
import Arkham.Helpers.Query as X
import Arkham.Helpers.SkillTest as X
import Arkham.Placement as X
import Arkham.Source as X
import Arkham.Target as X
import Arkham.Treachery.Helpers as X
import Arkham.Treachery.Types as X

import Arkham.Ability.Type
import Arkham.Card
import Arkham.Id
import Arkham.Message qualified as Msg
import Arkham.Token
import Arkham.Window (mkAfter)
import Arkham.Window qualified as Window

addHiddenToHand :: InvestigatorId -> TreacheryAttrs -> Message
addHiddenToHand iid a = PlaceTreachery (toId a) (TreacheryInHandOf iid)

forcedOnElimination :: InvestigatorId -> AbilityType
forcedOnElimination = ForcedAbility . eliminationWindow

on :: Targetable target => TreacheryAttrs -> target -> Bool
on = flip treacheryOn

instance RunMessage TreacheryAttrs where
  runMessage msg a@TreacheryAttrs {..} = case msg of
    Msg.InvestigatorEliminated iid | InvestigatorTarget iid `elem` treacheryAttachedTarget a -> do
      push $ toDiscard GameSource a
      pure a
    Msg.InvestigatorEliminated iid | Just iid == treacheryOwner -> do
      push $ toDiscard GameSource a
      pure a
    PlaceTreachery tid placement | tid == treacheryId -> do
      case placement of
        TreacheryAttachedTo (InvestigatorTarget iid) -> do
          pushM $ checkWindows [mkAfter $ Window.EntersThreatArea iid (toCard a)]
        _ -> pure ()
      pure $ a & placementL .~ placement
    PlaceTokens _ (isTarget a -> True) token n -> do
      pure $ a & tokensL %~ addTokens token n
    PlaceEnemyInVoid eid | EnemyTarget eid `elem` treacheryAttachedTarget a -> do
      push $ toDiscard GameSource a
      pure a
    Discard miid _ (TreacheryTarget tid) | tid == treacheryId -> do
      pure $ a & discardedByL .~ miid
    Discarded target _ _ | target `elem` treacheryAttachedTarget a -> do
      push $ toDiscard GameSource a
      pure a
    After (Revelation iid (isSource a -> True)) -> do
      pushWhen
        (treacheryPlacement == TreacheryLimbo)
        (toDiscardBy iid iid a)
      pure $ a & resolvedL %~ insertSet iid
    RemoveAllAttachments source target -> do
      case a.placement of
        TreacheryAttachedTo attached | target == attached -> push $ toDiscard source a
        _ -> pure ()
      pure a
    RemoveAllCopiesOfCardFromGame _ cCode | cCode == toCardCode a -> do
      push $ RemoveTreachery (toId a)
      pure a
    RemoveAllCopiesOfEncounterCardFromGame cardMatcher | toCard a `cardMatch` cardMatcher -> do
      push $ RemoveTreachery (toId a)
      pure a
    _ -> pure a
