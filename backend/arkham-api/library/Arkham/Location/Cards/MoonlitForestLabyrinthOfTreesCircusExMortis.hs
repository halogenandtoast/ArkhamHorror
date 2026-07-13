module Arkham.Location.Cards.MoonlitForestLabyrinthOfTreesCircusExMortis (
  moonlitForestLabyrinthOfTreesCircusExMortis,
) where

import Arkham.Ability
import Arkham.Helpers.Message.Discard.Lifted (randomDiscard)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype MoonlitForestLabyrinthOfTreesCircusExMortis = MoonlitForestLabyrinthOfTreesCircusExMortis LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

moonlitForestLabyrinthOfTreesCircusExMortis :: LocationCard MoonlitForestLabyrinthOfTreesCircusExMortis
moonlitForestLabyrinthOfTreesCircusExMortis =
  locationWith
    MoonlitForestLabyrinthOfTreesCircusExMortis
    Cards.moonlitForestLabyrinthOfTreesCircusExMortis
    3
    (Static 2)
    connectsToAdjacent

instance HasAbilities MoonlitForestLabyrinthOfTreesCircusExMortis where
  getAbilities (MoonlitForestLabyrinthOfTreesCircusExMortis a) =
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

instance RunMessage MoonlitForestLabyrinthOfTreesCircusExMortis where
  runMessage msg l@(MoonlitForestLabyrinthOfTreesCircusExMortis attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      randomDiscard iid (attrs.ability 1)
      pure l
    _ -> MoonlitForestLabyrinthOfTreesCircusExMortis <$> liftRunMessage msg attrs
