module Arkham.Location.Cards.OtherworldlyMechanismsSluiceControl (otherworldlyMechanismsSluiceControl) where

import Arkham.Ability
import Arkham.Helpers.Query (getSetAsideCardsMatching)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.TheGrandVault.Helpers
import Arkham.Trait (Trait (Glyph))

newtype OtherworldlyMechanismsSluiceControl = OtherworldlyMechanismsSluiceControl LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

otherworldlyMechanismsSluiceControl :: LocationCard OtherworldlyMechanismsSluiceControl
otherworldlyMechanismsSluiceControl = location OtherworldlyMechanismsSluiceControl Cards.otherworldlyMechanismsSluiceControl 1 (Static 3)

instance HasAbilities OtherworldlyMechanismsSluiceControl where
  getAbilities (OtherworldlyMechanismsSluiceControl a) =
    extendRevealed
      a
      [ mkAbility a 1 $ forced $ RevealLocation #when You (be a)
      , restricted a 2 (Here <> thisExists a (not_ activatedLocation))
          $ actionAbilityWithCost (DamageCost (a.ability 2) YouTarget 1 <> HorrorCost (a.ability 2) YouTarget 1)
      ]

instance RunMessage OtherworldlyMechanismsSluiceControl where
  runMessage msg l@(OtherworldlyMechanismsSluiceControl attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      glyphs <- getSetAsideCardsMatching (#treachery <> CardWithTrait Glyph)
      for_ (nonEmpty glyphs) (sample >=> drawCard iid)
      pure l
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      activateLocation (attrs.ability 2) attrs.id
      pure l
    _ -> OtherworldlyMechanismsSluiceControl <$> liftRunMessage msg attrs
