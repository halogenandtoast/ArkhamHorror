module Arkham.Location.Cards.OrneLibrary_c2026 (orneLibrary_c2026) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (orneLibrary_c2026)
import Arkham.Location.Import.Lifted

newtype OrneLibrary_c2026 = OrneLibrary_c2026 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

orneLibrary_c2026 :: LocationCard OrneLibrary_c2026
orneLibrary_c2026 = location OrneLibrary_c2026 Cards.orneLibrary_c2026 4 (PerPlayer 1)

instance HasAbilities OrneLibrary_c2026 where
  getAbilities (OrneLibrary_c2026 a) =
    extendRevealed1 a
      $ playerLimit PerGame
      $ restricted a 1 (Here <> CanDrawCards)
      $ ActionAbility mempty Nothing (ActionCost 2)

instance RunMessage OrneLibrary_c2026 where
  runMessage msg l@(OrneLibrary_c2026 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawCards iid (attrs.ability 1) 3
      pure l
    _ -> OrneLibrary_c2026 <$> liftRunMessage msg attrs
