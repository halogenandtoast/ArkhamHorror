module Arkham.Homebrew.TheFallOfTheHouseOfUsher.Locations.ManorChapelRuined (manorChapelRuined) where

import Arkham.Ability
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Message.Lifted.Choose

newtype ManorChapelRuined = ManorChapelRuined LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

manorChapelRuined :: LocationCard ManorChapelRuined
manorChapelRuined = location ManorChapelRuined Cards.manorChapelRuined 3 (Static 0)

instance HasAbilities ManorChapelRuined where
  getAbilities (ManorChapelRuined a) =
    extendRevealed1 a
      $ scenarioI18n
      $ withI18nTooltip "manorChapelRuined.theMadTrist"
      $ skillTestAbility
      $ restricted a 1 (Here <> Remembered unearthedAnArchaicBook <> noMadTristHasEnteredPlay) actionAbility

instance RunMessage ManorChapelRuined where
  runMessage msg l@(ManorChapelRuined attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseBeginSkillTest sid iid (attrs.ability 1) iid [#willpower, #combat] (Fixed 4)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      takeControlOfSetAsideAsset iid =<< getSetAsideCard Assets.theMadTristShelterFromTheStorm
      pure l
    _ -> ManorChapelRuined <$> liftRunMessage msg attrs
