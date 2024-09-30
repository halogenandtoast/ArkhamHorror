module Arkham.Location.Cards.TempleOfTheUnion_177a (
  templeOfTheUnion_177a,
  TempleOfTheUnion_177a (..),
)
where

import Arkham.Ability
import Arkham.Key
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.DevilReef.Helpers

newtype TempleOfTheUnion_177a = TempleOfTheUnion_177a LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

templeOfTheUnion_177a :: LocationCard TempleOfTheUnion_177a
templeOfTheUnion_177a = locationWith TempleOfTheUnion_177a Cards.templeOfTheUnion_177a 5 (PerPlayer 1) connectsToAdjacent

instance HasAbilities TempleOfTheUnion_177a where
  getAbilities (TempleOfTheUnion_177a a) =
    noKeyAbilities
      $ extendRevealed
        a
        [ mkAbility a 1 $ forced $ RevealLocation #after Anyone (be a)
        , restricted
            a
            2
            ( Here
                <> exists (at_ (be a) <> InvestigatorWithKey RedKey)
                <> exists (at_ (be a) <> InvestigatorWithKey GreenKey)
            )
            $ FastAbility
            $ GroupClueCost (PerPlayer 2) (be a)
        ]

instance RunMessage TempleOfTheUnion_177a where
  runMessage msg l@(TempleOfTheUnion_177a attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeKey attrs BlackKey
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      flashback iid Flashback11
      pure l
    _ -> TempleOfTheUnion_177a <$> liftRunMessage msg attrs
