module Arkham.Location.Cards.SubterraneanSwamp (subterraneanSwamp) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype SubterraneanSwamp = SubterraneanSwamp LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

subterraneanSwamp :: LocationCard SubterraneanSwamp
subterraneanSwamp = location SubterraneanSwamp Cards.subterraneanSwamp 0 (Static 0)

instance HasAbilities SubterraneanSwamp where
  getAbilities (SubterraneanSwamp attrs) =
    extendRevealed attrs []

instance RunMessage SubterraneanSwamp where
  runMessage msg (SubterraneanSwamp attrs) = runQueueT $ case msg of
    _ -> SubterraneanSwamp <$> liftRunMessage msg attrs
