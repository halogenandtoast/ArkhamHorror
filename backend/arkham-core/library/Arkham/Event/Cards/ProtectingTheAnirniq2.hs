module Arkham.Event.Cards.ProtectingTheAnirniq2 (
  protectingTheAnirniq2,
  ProtectingTheAnirniq2 (..),
)
where

import Arkham.Asset.Types (Field (..))
import Arkham.Capability
import Arkham.Draw.Types
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Projection
import Arkham.Window qualified as W
import Data.Monoid (First (..))

newtype ProtectingTheAnirniq2 = ProtectingTheAnirniq2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

protectingTheAnirniq2 :: EventCard ProtectingTheAnirniq2
protectingTheAnirniq2 = event ProtectingTheAnirniq2 Cards.protectingTheAnirniq2

instance RunMessage ProtectingTheAnirniq2 where
  runMessage msg e@(ProtectingTheAnirniq2 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      fc <-
        foldMapM
          ( \case
              W.AssetDefeated aid _ -> First . Just <$> field AssetCard aid
              W.Discarded _ _ c -> pure (First $ Just c)
              _ -> pure $ First Nothing
          )
          (map W.windowType attrs.windows)
      case getFirst fc of
        Nothing -> error "missing card"
        Just c -> for_ c.owner \owner -> do
          canDraw <- can.draw.cards owner
          canReturn <- can.have.cards.leaveDiscard owner
          when (canDraw || canReturn) do
            chooseOne iid
              $ [Label "Return that asset to its owner's hand" [AddToHand owner [c]] | canReturn]
              <> [Label "Its owner draws 3 cards" [DrawCards owner (newCardDraw attrs iid 3)] | canDraw]
      pure e
    _ -> ProtectingTheAnirniq2 <$> liftRunMessage msg attrs
