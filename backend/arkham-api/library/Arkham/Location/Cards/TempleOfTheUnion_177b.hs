module Arkham.Location.Cards.TempleOfTheUnion_177b (
  templeOfTheUnion_177b,
  TempleOfTheUnion_177b (..),
)
where

import Arkham.Ability
import Arkham.Key
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.DevilReef.Helpers

newtype TempleOfTheUnion_177b = TempleOfTheUnion_177b LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

templeOfTheUnion_177b :: LocationCard TempleOfTheUnion_177b
templeOfTheUnion_177b = locationWith TempleOfTheUnion_177b Cards.templeOfTheUnion_177b 3 (PerPlayer 2) connectsToAdjacent

instance HasAbilities TempleOfTheUnion_177b where
  getAbilities (TempleOfTheUnion_177b a) =
    noKeyAbilities
      $ extendRevealed
        a
        [ mkAbility a 1 $ forced $ RevealLocation #after Anyone (be a)
        , restricted
            a
            2
            ( Here
                <> exists (at_ (be a) <> InvestigatorWithKey BlueKey)
                <> exists (at_ (be a) <> InvestigatorWithKey YellowKey)
            )
            $ FastAbility
            $ GroupClueCost (PerPlayer 3) (be a)
        ]

instance RunMessage TempleOfTheUnion_177b where
  runMessage msg l@(TempleOfTheUnion_177b attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeKey attrs BlackKey
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      flashback iid Flashback11
      pure l
    _ -> TempleOfTheUnion_177b <$> liftRunMessage msg attrs
