module Arkham.Location.Cards.Easttown_c2026 (easttown_c2026) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (easttown_c2026)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Strategy

newtype Easttown_c2026 = Easttown_c2026 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

easttown_c2026 :: LocationCard Easttown_c2026
easttown_c2026 = location Easttown_c2026 Cards.easttown_c2026 2 (PerPlayer 1)

instance HasAbilities Easttown_c2026 where
  getAbilities (Easttown_c2026 a) =
    extendRevealed1 a
      $ playerLimit PerGame
      $ restricted a 1 Here
      $ freeReaction
      $ DiscoveringLastClue #after You (be a)

instance RunMessage Easttown_c2026 where
  runMessage msg l@(Easttown_c2026 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      search iid (attrs.ability 1) iid [fromDeck] (basic $ #asset <> #ally) (PlayFound iid 1)
      pure l
    _ -> Easttown_c2026 <$> liftRunMessage msg attrs
