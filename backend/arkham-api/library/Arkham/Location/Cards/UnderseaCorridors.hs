module Arkham.Location.Cards.UnderseaCorridors (underseaCorridors, UnderseaCorridors (..)) where

import Arkham.Ability
import Arkham.Key
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.FloodLevel
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype UnderseaCorridors = UnderseaCorridors LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

underseaCorridors :: LocationCard UnderseaCorridors
underseaCorridors =
  locationWith UnderseaCorridors Cards.underseaCorridors 0 (Static 0)
    $ connectsToAdjacent
    . (floodLevelL ?~ PartiallyFlooded)

instance HasAbilities UnderseaCorridors where
  getAbilities (UnderseaCorridors a) =
    extendRevealed
      a
      [ restricted a 1 (Here <> youExist (InvestigatorWithKey WhiteKey)) actionAbility
      , playerLimit PerRound
          $ restricted a 2 (thisExists a (LocationWithKey WhiteKey))
          $ FastAbility' Free [#move]
      ]

instance RunMessage UnderseaCorridors where
  runMessage msg l@(UnderseaCorridors attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      placeKey attrs WhiteKey
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      connecting <- select $ ConnectedFrom (be attrs)
      chooseTargetM iid connecting $ moveTo (attrs.ability 2) iid
      pure l
    _ -> UnderseaCorridors <$> liftRunMessage msg attrs
