module Arkham.Homebrew.NightOfVespers.Locations.PresidentialSuite_027 (presidentialSuite_027) where

import Arkham.Ability
import Arkham.Homebrew.NightOfVespers.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype PresidentialSuite_027 = PresidentialSuite_027 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

presidentialSuite_027 :: LocationCard PresidentialSuite_027
presidentialSuite_027 = symbolLabel $ location PresidentialSuite_027 Cards.presidentialSuite_027 4 (PerPlayer 2)

-- Printed on the unrevealed side but offered on both, because the window resolves
-- after this location is revealed. It rides the after-move window so that the
-- act's after-enter removal of unrevealed locations resolves first.
instance HasAbilities PresidentialSuite_027 where
  getAbilities (PresidentialSuite_027 a) =
    extend a
      $ mkAbility a 1 (forced $ Moves #after You AnySource Anywhere (be a))
      : (guard a.revealed *> [skillTestAbility $ mkAbility a 2 $ forced $ RevealLocation #when You (be a)])

instance RunMessage PresidentialSuite_027 where
  runMessage msg l@(PresidentialSuite_027 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeSetAsideLocationsMatching_
        $ mapOneOf
          cardIs
          [ Cards.hotelRooftop
          , Cards.hedgeMaze
          ]
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 2) iid #agility (Fixed 4)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      gainResources iid (attrs.ability 2) 1
      drawCards iid (attrs.ability 2) 1
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      loseActions iid (attrs.ability 2) 1
      pure l
    _ -> PresidentialSuite_027 <$> liftRunMessage msg attrs
