module Arkham.Location.Cards.CyclopeanRuins_176a (
  cyclopeanRuins_176a,
  CyclopeanRuins_176a (..),
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

newtype CyclopeanRuins_176a = CyclopeanRuins_176a LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cyclopeanRuins_176a :: LocationCard CyclopeanRuins_176a
cyclopeanRuins_176a = locationWith CyclopeanRuins_176a Cards.cyclopeanRuins_176a 3 (PerPlayer 1) connectsToAdjacent

instance HasAbilities CyclopeanRuins_176a where
  getAbilities (CyclopeanRuins_176a a) =
    noKeyAbilities
      $ extendRevealed
        a
        [ mkAbility a 1 $ forced $ RevealLocation #after Anyone (be a)
        , restricted
            a
            2
            ( Here
                <> exists (at_ (be a) <> InvestigatorWithKey RedKey)
                <> exists (at_ (be a) <> InvestigatorWithKey YellowKey)
            )
            $ FastAbility
            $ GroupClueCost (PerPlayer 2) (be a)
        ]

instance RunMessage CyclopeanRuins_176a where
  runMessage msg l@(CyclopeanRuins_176a attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      setThisFloodLevel attrs FullyFlooded
      placeKey attrs WhiteKey
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      flashback iid Flashback10
      pure l
    _ -> CyclopeanRuins_176a <$> liftRunMessage msg attrs
