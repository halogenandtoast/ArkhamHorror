module Arkham.Homebrew.NightOfVespers.Locations.PleasureGardens_026 (pleasureGardens_026) where

import Arkham.Ability
import Arkham.Homebrew.NightOfVespers.CardDefs.Locations qualified as Cards
import Arkham.I18n
import Arkham.Investigator.Types (Field (InvestigatorClues))
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection

newtype PleasureGardens_026 = PleasureGardens_026 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pleasureGardens_026 :: LocationCard PleasureGardens_026
pleasureGardens_026 = symbolLabel $ location PleasureGardens_026 Cards.pleasureGardens_026 3 (PerPlayer 2)

-- Printed on the unrevealed side but offered on both, because the window resolves
-- after this location is revealed. It rides the after-move window so that the
-- act's after-enter removal of unrevealed locations resolves first.
instance HasAbilities PleasureGardens_026 where
  getAbilities (PleasureGardens_026 a) =
    extend a
      $ mkAbility a 1 (forced $ Moves #after You AnySource Anywhere (be a))
      : (guard a.revealed *> [skillTestAbility $ mkAbility a 2 $ forced $ RevealLocation #when You (be a)])

instance RunMessage PleasureGardens_026 where
  runMessage msg l@(PleasureGardens_026 attrs) = runQueueT $ case msg of
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
      beginSkillTest sid iid (attrs.ability 2) iid #willpower (Fixed 3)
      pure l
    FailedThisSkillTestBy _ (isAbilitySource attrs 2 -> True) n -> do
      doStep n msg
      pure l
    DoStep n msg'@(FailedThisSkillTestBy iid (isAbilitySource attrs 2 -> True) _) -> do
      when (n > 0) do
        clues <- field InvestigatorClues iid
        chooseOrRunOneM iid $ withI18n do
          when (clues > 0)
            $ countVar 1
            $ labeled "placeCluesOnYourLocation"
            $ placeCluesOnLocation iid (attrs.ability 2) 1
          labeled "takeOneDamage" $ assignDamage iid (attrs.ability 2) 1
        doStep (n - 1) msg'
      pure l
    _ -> PleasureGardens_026 <$> liftRunMessage msg attrs
