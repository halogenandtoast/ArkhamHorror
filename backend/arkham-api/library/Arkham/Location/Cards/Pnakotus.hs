module Arkham.Location.Cards.Pnakotus (pnakotus) where

import Arkham.Ability
import Arkham.Discover
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection

newtype Pnakotus = Pnakotus LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pnakotus :: LocationCard Pnakotus
pnakotus = location Pnakotus Cards.pnakotus 2 (Static 3)

instance HasAbilities Pnakotus where
  getAbilities (Pnakotus a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted a 1 (Here <> ChaosTokenCountIs (IncludeSealed #tablet) (atLeast 3)) actionAbility

instance RunMessage Pnakotus where
  runMessage msg l@(Pnakotus attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      clues <- field LocationClues (toId l)
      discoverAt NotInvestigate iid (attrs.ability 1) clues attrs
      drawCards iid (attrs.ability 1) clues
      pure l
    _ -> Pnakotus <$> liftRunMessage msg attrs
