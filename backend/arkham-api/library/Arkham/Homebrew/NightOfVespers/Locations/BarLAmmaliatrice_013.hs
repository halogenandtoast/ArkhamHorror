module Arkham.Homebrew.NightOfVespers.Locations.BarLAmmaliatrice_013 (barLAmmaliatrice_013) where

import Arkham.Ability
import Arkham.Helpers.Investigator (canHaveHorrorHealed)
import Arkham.Helpers.Message.Discard.Lifted (chooseAndDiscardCard)
import Arkham.Homebrew.NightOfVespers.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype BarLAmmaliatrice_013 = BarLAmmaliatrice_013 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

barLAmmaliatrice_013 :: LocationCard BarLAmmaliatrice_013
barLAmmaliatrice_013 = symbolLabel $ location BarLAmmaliatrice_013 Cards.barLAmmaliatrice_013 2 (PerPlayer 1)

-- Printed on the unrevealed side but offered on both, because the window resolves
-- after this location is revealed. It rides the after-move window so that the
-- act's after-enter removal of unrevealed locations resolves first.
instance HasAbilities BarLAmmaliatrice_013 where
  getAbilities (BarLAmmaliatrice_013 a) =
    extend a
      $ mkAbility a 1 (forced $ Moves #after You AnySource Anywhere (be a))
      : ( guard a.revealed
            *> [ mkAbility a 2 $ forced $ RevealLocation #when You (be a)
               , restricted a 3 (Here <> exists (HealableInvestigator (a.ability 3) #horror You)) actionAbility
               ]
        )

instance RunMessage BarLAmmaliatrice_013 where
  runMessage msg l@(BarLAmmaliatrice_013 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeSetAsideLocationsMatching_
        $ mapOneOf
          cardIs
          [ Cards.galerieLavandiere_015
          , Cards.galerieLavandiere_016
          , Cards.mezzanineElevators_023
          , Cards.mezzanineElevators_024
          ]
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      chooseAndDiscardCard iid (attrs.ability 2)
      drawCards iid (attrs.ability 2) 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 3 -> do
      let source = attrs.ability 3
      whenM (canHaveHorrorHealed source iid) $ healHorror iid source 1
      pure l
    _ -> BarLAmmaliatrice_013 <$> liftRunMessage msg attrs
