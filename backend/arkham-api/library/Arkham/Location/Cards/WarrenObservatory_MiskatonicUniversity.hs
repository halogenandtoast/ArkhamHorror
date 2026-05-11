{- HLINT ignore "Use camelCase" -}
module Arkham.Location.Cards.WarrenObservatory_MiskatonicUniversity (warrenObservatory_MiskatonicUniversity) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Location.Cards qualified as Cards (warrenObservatory_MiskatonicUniversity)
import Arkham.Location.Import.Lifted

newtype WarrenObservatory_MiskatonicUniversity = WarrenObservatory_MiskatonicUniversity LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

warrenObservatory_MiskatonicUniversity :: LocationCard WarrenObservatory_MiskatonicUniversity
warrenObservatory_MiskatonicUniversity = location WarrenObservatory_MiskatonicUniversity Cards.warrenObservatory_MiskatonicUniversity 3 (PerPlayer 1)

instance HasAbilities WarrenObservatory_MiskatonicUniversity where
  getAbilities (WarrenObservatory_MiskatonicUniversity a) =
    extendRevealed1 a
      $ playerLimit PerRound
      $ restricted a 1 Here
      $ freeReaction
      $ DiscoverClues #after You (be a) (atLeast 1)

instance RunMessage WarrenObservatory_MiskatonicUniversity where
  runMessage msg l@(WarrenObservatory_MiskatonicUniversity attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawCards iid (attrs.ability 1) 1
      pure l
    _ -> WarrenObservatory_MiskatonicUniversity <$> liftRunMessage msg attrs
