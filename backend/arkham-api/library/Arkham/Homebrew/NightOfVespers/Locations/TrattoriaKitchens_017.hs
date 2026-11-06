module Arkham.Homebrew.NightOfVespers.Locations.TrattoriaKitchens_017 (trattoriaKitchens_017) where

import Arkham.Ability
import Arkham.Helpers.Act (getCurrentActStep)
import Arkham.Helpers.Investigator (canHaveDamageHealed)
import Arkham.Homebrew.NightOfVespers.CardDefs.Locations qualified as Cards
import Arkham.I18n
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype TrattoriaKitchens_017 = TrattoriaKitchens_017 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trattoriaKitchens_017 :: LocationCard TrattoriaKitchens_017
trattoriaKitchens_017 = symbolLabel $ location TrattoriaKitchens_017 Cards.trattoriaKitchens_017 1 (PerPlayer 2)

-- Printed on the unrevealed side but offered on both, because the window resolves
-- after this location is revealed. It rides the after-move window so that the
-- act's after-enter removal of unrevealed locations resolves first.
instance HasAbilities TrattoriaKitchens_017 where
  getAbilities (TrattoriaKitchens_017 a) =
    extend a
      $ mkAbility a 1 (forced $ Moves #after You AnySource Anywhere (be a))
      : ( guard a.revealed
            *> [ skillTestAbility $ mkAbility a 2 $ forced $ RevealLocation #when You (be a)
               , restricted a 3 (Here <> exists (HealableInvestigator (a.ability 3) #damage You)) actionAbility
               ]
        )

instance RunMessage TrattoriaKitchens_017 where
  runMessage msg l@(TrattoriaKitchens_017 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeSetAsideLocationsMatching_
        $ mapOneOf
          cardIs
          [ Cards.galerieLavandiere_015
          , Cards.galerieLavandiere_016
          , Cards.hotelWineCellar_029
          , Cards.hotelWineCellar_030
          ]
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      currentAct <- getCurrentActStep
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 2) iid #agility (Fixed $ 2 + currentAct)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      selectEach (affectsColocated iid) \iid' -> chooseOneM iid' $ withI18n do
        labeled "drawCardOne" $ drawCards iid' (attrs.ability 2) 1
        labeled "doNotDraw" nothing
      pure l
    UseThisAbility iid (isSource attrs -> True) 3 -> do
      let source = attrs.ability 3
      whenM (canHaveDamageHealed source iid) $ healDamage iid source 1
      pure l
    _ -> TrattoriaKitchens_017 <$> liftRunMessage msg attrs
