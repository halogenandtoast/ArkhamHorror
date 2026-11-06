module Arkham.Homebrew.NightOfVespers.Locations.PleasureGardens_025 (pleasureGardens_025) where

import Arkham.Ability
import Arkham.Homebrew.NightOfVespers.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype PleasureGardens_025 = PleasureGardens_025 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pleasureGardens_025 :: LocationCard PleasureGardens_025
pleasureGardens_025 = symbolLabel $ location PleasureGardens_025 Cards.pleasureGardens_025 3 (PerPlayer 2)

-- Printed on the unrevealed side but offered on both, because the window resolves
-- after this location is revealed. It rides the after-move window so that the
-- act's after-enter removal of unrevealed locations resolves first.
instance HasAbilities PleasureGardens_025 where
  getAbilities (PleasureGardens_025 a) =
    extend a
      $ mkAbility a 1 (forced $ Moves #after You AnySource Anywhere (be a))
      : (guard a.revealed *> [skillTestAbility $ mkAbility a 2 $ forced $ RevealLocation #when You (be a)])

instance RunMessage PleasureGardens_025 where
  runMessage msg l@(PleasureGardens_025 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeSetAsideLocationsMatching_
        $ mapOneOf
          cardIs
          [ Cards.hedgeMaze
          , Cards.hotelRooftop
          ]
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      chooseOneM iid $ for_ [#willpower, #agility] \kind ->
        skillLabeled kind $ beginSkillTest sid iid (attrs.ability 2) iid kind (Fixed 3)
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      chooseAndDiscardAsset iid (attrs.ability 2)
      pure l
    _ -> PleasureGardens_025 <$> liftRunMessage msg attrs
