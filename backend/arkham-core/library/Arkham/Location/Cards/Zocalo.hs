module Arkham.Location.Cards.Zocalo (zocalo, Zocalo (..)) where

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype Zocalo = Zocalo LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

zocalo :: LocationCard Zocalo
zocalo = locationWith Zocalo Cards.zocalo 3 (Static 0) (labelL .~ "diamond")

instance HasAbilities Zocalo where
  getAbilities (Zocalo attrs) =
    withRevealedAbilities attrs [restrictedAbility attrs 1 Here $ exploreAction $ DiscardCombinedCost 5]

instance RunMessage Zocalo where
  runMessage msg l@(Zocalo attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = toAbilitySource attrs 1
      push $ Explore iid source $ CardWithPrintedLocationSymbol $ locationSymbol attrs
      pure l
    _ -> Zocalo <$> runMessage msg attrs
