{- HLINT ignore "Use camelCase" -}
module Arkham.Location.Cards.OrneLibrary_MiskatonicUniversity (orneLibrary_MiskatonicUniversity) where

import Arkham.Ability
import Arkham.Capability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (orneLibrary_MiskatonicUniversity)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype OrneLibrary_MiskatonicUniversity = OrneLibrary_MiskatonicUniversity LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

orneLibrary_MiskatonicUniversity :: LocationCard OrneLibrary_MiskatonicUniversity
orneLibrary_MiskatonicUniversity = location OrneLibrary_MiskatonicUniversity Cards.orneLibrary_MiskatonicUniversity 4 (PerPlayer 1)

instance HasAbilities OrneLibrary_MiskatonicUniversity where
  getAbilities (OrneLibrary_MiskatonicUniversity a) =
    extendRevealed1 a
      $ playerLimit PerGame
      $ restricted a 1 (Here <> can.draw.cards You) doubleActionAbility

instance RunMessage OrneLibrary_MiskatonicUniversity where
  runMessage msg l@(OrneLibrary_MiskatonicUniversity attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawCards iid (attrs.ability 1) 3
      pure l
    _ -> OrneLibrary_MiskatonicUniversity <$> liftRunMessage msg attrs
