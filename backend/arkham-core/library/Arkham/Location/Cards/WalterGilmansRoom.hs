module Arkham.Location.Cards.WalterGilmansRoom (walterGilmansRoom, WalterGilmansRoom (..)) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype WalterGilmansRoom = WalterGilmansRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

walterGilmansRoom :: LocationCard WalterGilmansRoom
walterGilmansRoom =
  locationWith WalterGilmansRoom Cards.walterGilmansRoom 4 (PerPlayer 1)
    $ costToEnterUnrevealedL
    .~ Costs [ActionCost 1, GroupClueCost (PerPlayer 1) (locationIs Locations.moldyHalls)]

instance HasAbilities WalterGilmansRoom where
  getAbilities (WalterGilmansRoom a) =
    withRevealedAbilities
      a
      [ restrictedAbility a 1 Here actionAbility
      , haunted "Discard the top 2 cards of the encounter deck." a 2
      ]

instance RunMessage WalterGilmansRoom where
  runMessage msg l@(WalterGilmansRoom attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      mDrawing <- drawCardsIfCan iid (attrs.ability 1) 3
      pushAll $ toList mDrawing <> [assignHorror iid (attrs.ability 1) 1]
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push $ DiscardTopOfEncounterDeck iid 2 (attrs.ability 2) Nothing
      pure l
    _ -> WalterGilmansRoom <$> runMessage msg attrs
