module Arkham.Homebrew.NightOfVespers.Locations.HotelWineCellar_029 (hotelWineCellar_029) where

import Arkham.Ability
import Arkham.Homebrew.NightOfVespers.CardDefs.Locations qualified as Cards
import Arkham.I18n
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Strategy

newtype HotelWineCellar_029 = HotelWineCellar_029 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hotelWineCellar_029 :: LocationCard HotelWineCellar_029
hotelWineCellar_029 =
  symbolLabel $ location HotelWineCellar_029 Cards.hotelWineCellar_029 3 (PerPlayer 2)

-- Printed on the unrevealed side but offered on both, because the window resolves
-- after this location is revealed. It rides the after-move window so that the
-- act's after-enter removal of unrevealed locations resolves first.
instance HasAbilities HotelWineCellar_029 where
  getAbilities (HotelWineCellar_029 a) =
    extend a
      $ mkAbility a 1 (forced $ Moves #after You AnySource Anywhere (be a))
      : (guard a.revealed *> [mkAbility a 2 $ forced $ RevealLocation #when You (be a)])

instance RunMessage HotelWineCellar_029 where
  runMessage msg l@(HotelWineCellar_029 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeSetAsideLocationsMatching_
        $ mapOneOf
          cardIs
          [ Cards.hotelLobby_019
          , Cards.hotelLobby_020
          , Cards.pleasureGardens_025
          , Cards.pleasureGardens_026
          ]
      pure l
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      eachInvestigator \iid -> chooseOneM iid $ withI18n do
        labeled "takeOneHorror" $ doStep 1 (ForInvestigator iid msg)
        labeled "doNothing" nothing
      pure l
    DoStep 1 (ForInvestigator iid (UseThisAbility _ (isSource attrs -> True) 2)) -> do
      assignHorror iid (attrs.ability 2) 1
      search iid (attrs.ability 2) iid [fromDiscard] #any (AddFoundToHand iid 1)
      pure l
    _ -> HotelWineCellar_029 <$> liftRunMessage msg attrs
