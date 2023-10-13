{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Treachery.Runner (
  module X,
  addHiddenToHand,
  forcedOnElimination,
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
import Arkham.Id
import Arkham.Message qualified as Msg
import Arkham.Token

addHiddenToHand :: InvestigatorId -> TreacheryAttrs -> Message
addHiddenToHand iid a = PlaceTreachery (toId a) (TreacheryInHandOf iid)

forcedOnElimination :: InvestigatorId -> AbilityType
forcedOnElimination = ForcedAbility . eliminationWindow

instance RunMessage TreacheryAttrs where
  runMessage msg a@TreacheryAttrs {..} = case msg of
    Msg.InvestigatorEliminated iid | InvestigatorTarget iid `elem` treacheryAttachedTarget a -> do
      push (Discard GameSource $ toTarget a)
      pure a
    Msg.InvestigatorEliminated iid | Just iid == treacheryOwner -> do
      a <$ push (Discard GameSource $ toTarget a)
    PlaceTreachery tid placement | tid == treacheryId -> do
      pure $ a & placementL .~ placement
    PlaceTokens _ (isTarget a -> True) token n -> do
      pure $ a & tokensL %~ addTokens token n
    PlaceEnemyInVoid eid | EnemyTarget eid `elem` treacheryAttachedTarget a -> do
      a <$ push (Discard GameSource $ toTarget a)
    Discarded target _ _ | target `elem` treacheryAttachedTarget a -> do
      a <$ push (Discard GameSource $ toTarget a)
    After (Revelation _ (isSource a -> True)) -> do
      when
        (treacheryPlacement == TreacheryLimbo)
        (push $ Discard GameSource $ toTarget a)
      pure a
    _ -> pure a
