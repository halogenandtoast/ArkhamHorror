module Arkham.Homebrew.NightOfVespers.Locations.HotelWineCellar_030 (hotelWineCellar_030) where

import Arkham.Ability
import Arkham.Homebrew.NightOfVespers.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype HotelWineCellar_030 = HotelWineCellar_030 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hotelWineCellar_030 :: LocationCard HotelWineCellar_030
hotelWineCellar_030 =
  symbolLabel $ location HotelWineCellar_030 Cards.hotelWineCellar_030 3 (PerPlayer 2)

-- Printed on the unrevealed side but offered on both, because the window resolves
-- after this location is revealed. It rides the after-move window so that the
-- act's after-enter removal of unrevealed locations resolves first.
instance HasAbilities HotelWineCellar_030 where
  getAbilities (HotelWineCellar_030 a) =
    extend a
      $ mkAbility a 1 (forced $ Moves #after You AnySource Anywhere (be a))
      : (guard a.revealed *> [skillTestAbility $ mkAbility a 2 $ forced $ RevealLocation #when You (be a)])

instance RunMessage HotelWineCellar_030 where
  runMessage msg l@(HotelWineCellar_030 attrs) = runQueueT $ case msg of
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
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      chooseOneM iid $ for_ [#intellect, #agility] \kind ->
        skillLabeled kind $ beginSkillTest sid iid (attrs.ability 2) iid kind (Fixed 3)
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      anyEnemies <- selectAny AnyEnemy
      if anyEnemies
        then do
          nearest <- select $ NearestEnemyToLocation attrs.id AnyEnemy
          unless (null nearest)
            $ chooseTargetM iid nearest \enemy -> enemyMoveTo (attrs.ability 2) enemy attrs.id
        else assignDamageAndHorror iid (attrs.ability 2) 1 1
      pure l
    _ -> HotelWineCellar_030 <$> liftRunMessage msg attrs
