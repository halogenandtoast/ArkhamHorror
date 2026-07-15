module Arkham.Homebrew.CircusExMortis.Locations.GamesGallery (gamesGallery) where

import Arkham.Ability
import Arkham.Capability
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Strategy
import Arkham.Trait (Trait (Item))

newtype GamesGallery = GamesGallery LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gamesGallery :: LocationCard GamesGallery
gamesGallery =
  location GamesGallery Cards.gamesGallery 4 (PerPlayer 1)

instance HasAbilities GamesGallery where
  getAbilities (GamesGallery a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted a 1 (Here <> NoCluesOnThis <> can.search.deck You) actionAbility

instance RunMessage GamesGallery where
  runMessage msg l@(GamesGallery attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      search iid (attrs.ability 1) iid [fromTopOfDeck 9] (basic $ #asset <> CardWithTrait Item) (DrawFound iid 1)
      pure l
    _ -> GamesGallery <$> liftRunMessage msg attrs
