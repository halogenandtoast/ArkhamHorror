module Arkham.Location.Cards.DeepOneGrotto_175b (
  deepOneGrotto_175b,
  DeepOneGrotto_175b (..),
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

newtype DeepOneGrotto_175b = DeepOneGrotto_175b LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deepOneGrotto_175b :: LocationCard DeepOneGrotto_175b
deepOneGrotto_175b = locationWith DeepOneGrotto_175b Cards.deepOneGrotto_175b 2 (PerPlayer 2) connectsToAdjacent

instance HasAbilities DeepOneGrotto_175b where
  getAbilities (DeepOneGrotto_175b a) =
    noKeyAbilities
      $ extendRevealed
        a
        [ mkAbility a 1 $ forced $ RevealLocation #after Anyone (be a)
        , restricted
            a
            2
            ( Here
                <> exists (at_ (be a) <> InvestigatorWithKey GreenKey)
                <> exists (at_ (be a) <> InvestigatorWithKey YellowKey)
            )
            $ FastAbility
            $ GroupClueCost (PerPlayer 3) (be a)
        ]

instance RunMessage DeepOneGrotto_175b where
  runMessage msg l@(DeepOneGrotto_175b attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      increaseThisFloodLevel attrs
      placeKey attrs PurpleKey
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      flashback iid Flashback9
      pure l
    _ -> DeepOneGrotto_175b <$> liftRunMessage msg attrs
