module Arkham.Homebrew.AgesUnwound.Locations.SideBuilding_170 (sideBuilding_170) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype SideBuilding_170 = SideBuilding_170 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sideBuilding_170 :: LocationCard SideBuilding_170
sideBuilding_170 = symbolLabel $ location SideBuilding_170 Cards.sideBuilding_170 3 (PerPlayer 1)

{- | "This location's shroud cannot be reduced."

'ShroudCannotBeReduced' floors the 'ShroudModifier' fold at the base value in
@getModifiedShroudValueFor@, so a Flashlight (or this scenario's own Children's
Playground ability) cannot lower it while the Forced below can still raise it.
-}
instance HasModifiersFor SideBuilding_170 where
  getModifiersFor (SideBuilding_170 a) = modifySelf a [ShroudCannotBeReduced]

{- | "Forced - After you successfully investigate Side Building: Side Building
gets +2 shroud until the end of the round."

Two, not Scenario III's one: VI gathers its own Side Building and the later
printing raises the penalty.
-}
instance HasAbilities SideBuilding_170 where
  getAbilities (SideBuilding_170 a) =
    extendRevealed1 a $ forcedAbility a 1 $ SuccessfulInvestigation #after You (be a)

instance RunMessage SideBuilding_170 where
  runMessage msg l@(SideBuilding_170 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      roundModifier (attrs.ability 1) attrs (ShroudModifier 2)
      pure l
    _ -> SideBuilding_170 <$> liftRunMessage msg attrs
