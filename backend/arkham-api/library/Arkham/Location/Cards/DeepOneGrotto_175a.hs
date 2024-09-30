module Arkham.Location.Cards.DeepOneGrotto_175a (
  deepOneGrotto_175a,
  DeepOneGrotto_175a (..),
)
where

import Arkham.Ability
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Key
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.DevilReef.Helpers

newtype DeepOneGrotto_175a = DeepOneGrotto_175a LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deepOneGrotto_175a :: LocationCard DeepOneGrotto_175a
deepOneGrotto_175a = locationWith DeepOneGrotto_175a Cards.deepOneGrotto_175a 4 (PerPlayer 1) connectsToAdjacent

instance HasAbilities DeepOneGrotto_175a where
  getAbilities (DeepOneGrotto_175a a) =
    noKeyAbilities
      $ extendRevealed
        a
        [ mkAbility a 1 $ forced $ RevealLocation #after Anyone (be a)
        , restricted
            a
            2
            ( Here
                <> exists (at_ (be a) <> InvestigatorWithKey RedKey)
                <> exists (at_ (be a) <> InvestigatorWithKey BlueKey)
            )
            $ FastAbility
            $ GroupClueCost (PerPlayer 2) (be a)
        ]

instance RunMessage DeepOneGrotto_175a where
  runMessage msg l@(DeepOneGrotto_175a attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      increaseThisFloodLevel attrs
      placeKey attrs PurpleKey
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      flashback iid Flashback9
      pure l
    _ -> DeepOneGrotto_175a <$> liftRunMessage msg attrs
