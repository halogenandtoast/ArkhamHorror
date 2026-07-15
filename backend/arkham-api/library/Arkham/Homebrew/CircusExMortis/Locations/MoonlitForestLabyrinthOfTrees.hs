module Arkham.Homebrew.CircusExMortis.Locations.MoonlitForestLabyrinthOfTrees (
  moonlitForestLabyrinthOfTrees,
) where

import Arkham.Ability
import Arkham.Helpers.Message.Discard.Lifted (randomDiscard)
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype MoonlitForestLabyrinthOfTrees = MoonlitForestLabyrinthOfTrees LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

moonlitForestLabyrinthOfTrees :: LocationCard MoonlitForestLabyrinthOfTrees
moonlitForestLabyrinthOfTrees =
  locationWith
    MoonlitForestLabyrinthOfTrees
    Cards.moonlitForestLabyrinthOfTrees
    3
    (Static 2)
    connectsToAdjacent

instance HasAbilities MoonlitForestLabyrinthOfTrees where
  getAbilities (MoonlitForestLabyrinthOfTrees a) =
    -- "Forced - When an investigator at an adjacent copy of Moonlit Forest moves to a
    -- location other than this location: That investigator discards a card from their hand
    -- at random."
    extendRevealed1 a
      $ mkAbility a 1
      $ forced
      $ Moves
        #after
        Anyone
        AnySource
        (LocationWithTitle "Moonlit Forest" <> connectedTo (be a))
        (not_ $ be a)

instance RunMessage MoonlitForestLabyrinthOfTrees where
  runMessage msg l@(MoonlitForestLabyrinthOfTrees attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      randomDiscard iid (attrs.ability 1)
      pure l
    _ -> MoonlitForestLabyrinthOfTrees <$> liftRunMessage msg attrs
