module Arkham.Location.Cards.GamesGalleryCircusExMortis (gamesGalleryCircusExMortis) where

import Arkham.Ability
import Arkham.Capability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Strategy
import Arkham.Trait (Trait (Item))

newtype GamesGalleryCircusExMortis = GamesGalleryCircusExMortis LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gamesGalleryCircusExMortis :: LocationCard GamesGalleryCircusExMortis
gamesGalleryCircusExMortis =
  location GamesGalleryCircusExMortis Cards.gamesGalleryCircusExMortis 4 (PerPlayer 1)

instance HasAbilities GamesGalleryCircusExMortis where
  getAbilities (GamesGalleryCircusExMortis a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted a 1 (Here <> NoCluesOnThis <> can.search.deck You) actionAbility

instance RunMessage GamesGalleryCircusExMortis where
  runMessage msg l@(GamesGalleryCircusExMortis attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      search iid (attrs.ability 1) iid [fromTopOfDeck 9] (basic $ #asset <> CardWithTrait Item) (DrawFound iid 1)
      pure l
    _ -> GamesGalleryCircusExMortis <$> liftRunMessage msg attrs
