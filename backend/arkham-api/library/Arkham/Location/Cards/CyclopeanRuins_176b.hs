module Arkham.Location.Cards.CyclopeanRuins_176b (
  cyclopeanRuins_176b,
  CyclopeanRuins_176b (..),
)
where

import Arkham.Ability
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Key
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.FloodLevel
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.DevilReef.Helpers

newtype CyclopeanRuins_176b = CyclopeanRuins_176b LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cyclopeanRuins_176b :: LocationCard CyclopeanRuins_176b
cyclopeanRuins_176b = locationWith CyclopeanRuins_176b Cards.cyclopeanRuins_176b 1 (PerPlayer 2) connectsToAdjacent

instance HasAbilities CyclopeanRuins_176b where
  getAbilities (CyclopeanRuins_176b a) =
    noKeyAbilities
      $ extendRevealed
        a
        [ mkAbility a 1 $ forced $ RevealLocation #after Anyone (be a)
        , restricted
            a
            2
            ( Here
                <> exists (at_ (be a) <> InvestigatorWithKey BlueKey)
                <> exists (at_ (be a) <> InvestigatorWithKey GreenKey)
            )
            $ FastAbility
            $ GroupClueCost (PerPlayer 3) (be a)
        ]

instance RunMessage CyclopeanRuins_176b where
  runMessage msg l@(CyclopeanRuins_176b attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      setThisFloodLevel attrs FullyFlooded
      placeKey attrs WhiteKey
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      flashback iid Flashback10
      pure l
    _ -> CyclopeanRuins_176b <$> liftRunMessage msg attrs
