module Arkham.Homebrew.NightOfVespers.Locations.PresidentialSuite_028 (presidentialSuite_028) where

import Arkham.Ability
import Arkham.Homebrew.NightOfVespers.CardDefs.Locations qualified as Cards
import Arkham.Investigator.Types (Field (InvestigatorClues))
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Projection

newtype PresidentialSuite_028 = PresidentialSuite_028 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

presidentialSuite_028 :: LocationCard PresidentialSuite_028
presidentialSuite_028 =
  symbolLabel $ location PresidentialSuite_028 Cards.presidentialSuite_028 4 (PerPlayer 2)

-- Printed on the unrevealed side but offered on both, because the window resolves
-- after this location is revealed. It rides the after-move window so that the
-- act's after-enter removal of unrevealed locations resolves first.
instance HasAbilities PresidentialSuite_028 where
  getAbilities (PresidentialSuite_028 a) =
    extend a
      $ mkAbility a 1 (forced $ Moves #after You AnySource Anywhere (be a))
      : ( guard a.revealed
            *> [ mkAbility a 2 $ forced $ RevealLocation #when You (be a)
               , groupLimit PerGame $ restricted a 3 Here actionAbility
               ]
        )

instance RunMessage PresidentialSuite_028 where
  runMessage msg l@(PresidentialSuite_028 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeSetAsideLocationsMatching_
        $ mapOneOf
          cardIs
          [ Cards.hotelRooftop
          , Cards.hedgeMaze
          ]
      pure l
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      selectOne (affectsOthers MostClues) >>= traverse_ \iid -> do
        sid <- getRandom
        beginSkillTest sid iid (attrs.ability 2) attrs #agility (Fixed 3)
      pure l
    FailedThisSkillTestBy iid (isAbilitySource attrs 2 -> True) n -> do
      clues <- field InvestigatorClues iid
      if clues >= n
        then placeCluesOnLocation iid (attrs.ability 2) n
        else directDamageAndHorror iid (attrs.ability 2) 1 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 3 -> do
      gainResources iid (attrs.ability 3) 5
      pure l
    _ -> PresidentialSuite_028 <$> liftRunMessage msg attrs
