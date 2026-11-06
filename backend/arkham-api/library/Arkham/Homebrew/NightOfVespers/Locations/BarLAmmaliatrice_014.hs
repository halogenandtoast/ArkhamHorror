module Arkham.Homebrew.NightOfVespers.Locations.BarLAmmaliatrice_014 (barLAmmaliatrice_014) where

import Arkham.Ability
import Arkham.Homebrew.NightOfVespers.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier

newtype BarLAmmaliatrice_014 = BarLAmmaliatrice_014 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

barLAmmaliatrice_014 :: LocationCard BarLAmmaliatrice_014
barLAmmaliatrice_014 = symbolLabel $ location BarLAmmaliatrice_014 Cards.barLAmmaliatrice_014 2 (PerPlayer 1)

-- Printed on the unrevealed side but offered on both, because the window resolves
-- after this location is revealed. It rides the after-move window so that the
-- act's after-enter removal of unrevealed locations resolves first.
instance HasAbilities BarLAmmaliatrice_014 where
  getAbilities (BarLAmmaliatrice_014 a) =
    extend a
      $ mkAbility a 1 (forced $ Moves #after You AnySource Anywhere (be a))
      : ( guard a.revealed
            *> [ mkAbility a 2 $ forced $ RevealLocation #when You (be a)
               , investigateAbility a 3 mempty Here
               ]
        )

instance RunMessage BarLAmmaliatrice_014 where
  runMessage msg l@(BarLAmmaliatrice_014 attrs) = runQueueT $ case msg of
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
      assignHorror iid (attrs.ability 2) 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 3 -> do
      let source = attrs.ability 3
      sid <- getRandom
      skillTestModifier sid source iid (UseSkillInsteadOf #intellect #agility)
      investigate sid iid source
      pure l
    _ -> BarLAmmaliatrice_014 <$> liftRunMessage msg attrs
