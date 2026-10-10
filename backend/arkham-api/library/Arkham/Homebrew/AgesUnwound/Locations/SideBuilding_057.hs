module Arkham.Homebrew.AgesUnwound.Locations.SideBuilding_057 (sideBuilding_057) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype SideBuilding_057 = SideBuilding_057 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sideBuilding_057 :: LocationCard SideBuilding_057
sideBuilding_057 = symbolLabel $ location SideBuilding_057 Cards.sideBuilding_057 3 (PerPlayer 1)

{- | "This location's shroud cannot be reduced."

'ShroudCannotBeReduced' floors the 'ShroudModifier' fold at the base value in
@getModifiedShroudValueFor@, so a Flashlight (or this scenario's own Children's
Playground ability) cannot lower it while the Forced below can still raise it.
-}
instance HasModifiersFor SideBuilding_057 where
  getModifiersFor (SideBuilding_057 a) = modifySelf a [ShroudCannotBeReduced]

{- | "Forced - After you successfully investigate Side Building: Side Building
gets +1 shroud until the end of the round."
-}
instance HasAbilities SideBuilding_057 where
  getAbilities (SideBuilding_057 a) =
    extendRevealed1 a $ forcedAbility a 1 $ SuccessfulInvestigation #after You (be a)

instance RunMessage SideBuilding_057 where
  runMessage msg l@(SideBuilding_057 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      roundModifier (attrs.ability 1) attrs (ShroudModifier 1)
      pure l
    _ -> SideBuilding_057 <$> liftRunMessage msg attrs
