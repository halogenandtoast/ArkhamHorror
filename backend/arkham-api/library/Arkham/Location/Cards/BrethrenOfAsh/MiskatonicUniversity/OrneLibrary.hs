{- HLINT ignore "Use camelCase" -}
module Arkham.Location.Cards.BrethrenOfAsh.MiskatonicUniversity.OrneLibrary (orneLibrary) where

import Arkham.Ability
import Arkham.Capability
import Arkham.GameValue
import Arkham.Location.CardDefs.BrethrenOfAsh.MiskatonicUniversity qualified as Cards (
  orneLibrary,
 )
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype OrneLibrary = OrneLibrary LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

orneLibrary :: LocationCard OrneLibrary
orneLibrary = location OrneLibrary Cards.orneLibrary 4 (PerPlayer 1)

instance HasAbilities OrneLibrary where
  getAbilities (OrneLibrary a) =
    extendRevealed1 a
      $ playerLimit PerGame
      $ restricted a 1 (Here <> can.draw.cards You) doubleActionAbility

instance RunMessage OrneLibrary where
  runMessage msg l@(OrneLibrary attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawCards iid (attrs.ability 1) 3
      pure l
    _ -> OrneLibrary <$> liftRunMessage msg attrs
