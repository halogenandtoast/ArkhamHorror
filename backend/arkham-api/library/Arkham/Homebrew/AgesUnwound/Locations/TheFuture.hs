module Arkham.Homebrew.AgesUnwound.Locations.TheFuture (theFuture) where

import Arkham.Ability
import Arkham.Helpers.Message.Discard.Lifted (chooseAndDiscardCard)
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (revealedL)
import Arkham.Matcher

newtype TheFuture = TheFuture LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theFuture :: LocationCard TheFuture
theFuture =
  symbolLabel $ locationWith TheFuture Cards.theFuture 4 (PerPlayer 1) $ revealedL .~ True

{- | "__Forced__ - At the start of your turn: Choose and discard a card from your
hand. Draw a card."
-}
instance HasAbilities TheFuture where
  getAbilities (TheFuture a) =
    extendRevealed1 a $ restricted a 1 Here $ forced $ TurnBegins #when You

instance RunMessage TheFuture where
  runMessage msg l@(TheFuture attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseAndDiscardCard iid (attrs.ability 1)
      drawCards iid (attrs.ability 1) 1
      pure l
    _ -> TheFuture <$> liftRunMessage msg attrs
