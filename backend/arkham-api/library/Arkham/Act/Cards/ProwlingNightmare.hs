module Arkham.Act.Cards.ProwlingNightmare (prowlingNightmare) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype ProwlingNightmare = ProwlingNightmare ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

prowlingNightmare :: ActCard ProwlingNightmare
prowlingNightmare = act (3, A) ProwlingNightmare Cards.prowlingNightmare Nothing

instance RunMessage ProwlingNightmare where
  runMessage msg a@(ProwlingNightmare attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> ProwlingNightmare <$> liftRunMessage msg attrs
