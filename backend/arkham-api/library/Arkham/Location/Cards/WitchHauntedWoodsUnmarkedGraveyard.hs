module Arkham.Location.Cards.WitchHauntedWoodsUnmarkedGraveyard (witchHauntedWoodsUnmarkedGraveyard) where

import Arkham.Ability
import Arkham.Deck qualified as Deck
import Arkham.Helpers.Scenario
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype WitchHauntedWoodsUnmarkedGraveyard = WitchHauntedWoodsUnmarkedGraveyard LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

witchHauntedWoodsUnmarkedGraveyard :: LocationCard WitchHauntedWoodsUnmarkedGraveyard
witchHauntedWoodsUnmarkedGraveyard =
  location WitchHauntedWoodsUnmarkedGraveyard Cards.witchHauntedWoodsUnmarkedGraveyard 1 (PerPlayer 2)

instance HasAbilities WitchHauntedWoodsUnmarkedGraveyard where
  getAbilities (WitchHauntedWoodsUnmarkedGraveyard a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ DiscoveringLastClue #when You (be a)

instance RunMessage WitchHauntedWoodsUnmarkedGraveyard where
  runMessage msg l@(WitchHauntedWoodsUnmarkedGraveyard attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      findTopOfDiscard (#treachery <> #hex) >>= \case
        Nothing -> discardUntilFirst iid attrs Deck.EncounterDeck (#treachery <> #hex)
        Just hex -> drawCardFrom iid hex Deck.EncounterDeck
      pure l
    RequestedEncounterCard (isSource attrs -> True) (Just iid) (Just hex) -> do
      drawCardFrom iid hex Deck.EncounterDeck
      pure l
    _ -> WitchHauntedWoodsUnmarkedGraveyard <$> liftRunMessage msg attrs
