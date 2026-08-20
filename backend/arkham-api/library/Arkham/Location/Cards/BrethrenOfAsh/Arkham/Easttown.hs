{- HLINT ignore "Use camelCase" -}
module Arkham.Location.Cards.BrethrenOfAsh.Arkham.Easttown (easttown) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.CardDefs.BrethrenOfAsh.Arkham qualified as Cards (easttown)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Strategy

newtype Easttown = Easttown LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

easttown :: LocationCard Easttown
easttown = location Easttown Cards.easttown 2 (PerPlayer 1)

instance HasAbilities Easttown where
  getAbilities (Easttown a) =
    extendRevealed1 a
      $ playerLimit PerGame
      $ restricted a 1 Here
      $ freeReaction
      $ DiscoveringLastClue #after You (be a)

instance RunMessage Easttown where
  runMessage msg l@(Easttown attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      search iid (attrs.ability 1) iid [fromDeck] (basic $ #asset <> #ally) (PlayFound iid 1)
      pure l
    _ -> Easttown <$> liftRunMessage msg attrs
