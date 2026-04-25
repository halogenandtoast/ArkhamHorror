module Arkham.Location.Cards.WarrenObservatory_c2026 (warrenObservatory_c2026) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Location.Cards qualified as Cards (warrenObservatory_c2026)
import Arkham.Location.Import.Lifted

newtype WarrenObservatory_c2026 = WarrenObservatory_c2026 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

warrenObservatory_c2026 :: LocationCard WarrenObservatory_c2026
warrenObservatory_c2026 = location WarrenObservatory_c2026 Cards.warrenObservatory_c2026 3 (PerPlayer 1)

instance HasAbilities WarrenObservatory_c2026 where
  getAbilities (WarrenObservatory_c2026 a) =
    extendRevealed1 a
      $ playerLimit PerRound
      $ restricted a 1 Here
      $ freeReaction
      $ DiscoverClues #after You (be a) (atLeast 1)

instance RunMessage WarrenObservatory_c2026 where
  runMessage msg l@(WarrenObservatory_c2026 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawCards iid (attrs.ability 1) 1
      pure l
    _ -> WarrenObservatory_c2026 <$> liftRunMessage msg attrs
