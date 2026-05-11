{- HLINT ignore "Use camelCase" -}
module Arkham.Location.Cards.Easttown_Arkham (easttown_Arkham) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (easttown_Arkham)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Strategy

newtype Easttown_Arkham = Easttown_Arkham LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

easttown_Arkham :: LocationCard Easttown_Arkham
easttown_Arkham = location Easttown_Arkham Cards.easttown_Arkham 2 (PerPlayer 1)

instance HasAbilities Easttown_Arkham where
  getAbilities (Easttown_Arkham a) =
    extendRevealed1 a
      $ playerLimit PerGame
      $ restricted a 1 Here
      $ freeReaction
      $ DiscoveringLastClue #after You (be a)

instance RunMessage Easttown_Arkham where
  runMessage msg l@(Easttown_Arkham attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      search iid (attrs.ability 1) iid [fromDeck] (basic $ #asset <> #ally) (PlayFound iid 1)
      pure l
    _ -> Easttown_Arkham <$> liftRunMessage msg attrs
