module Arkham.Act.Cards.TheFinalMirage (theFinalMirage) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype TheFinalMirage = TheFinalMirage ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theFinalMirage :: ActCard TheFinalMirage
theFinalMirage = act (4, A) TheFinalMirage Cards.theFinalMirage Nothing

instance RunMessage TheFinalMirage where
  runMessage msg a@(TheFinalMirage attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> TheFinalMirage <$> liftRunMessage msg attrs
