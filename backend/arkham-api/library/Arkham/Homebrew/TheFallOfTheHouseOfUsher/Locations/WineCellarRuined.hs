module Arkham.Homebrew.TheFallOfTheHouseOfUsher.Locations.WineCellarRuined (wineCellarRuined) where

import Arkham.Ability
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.Helpers
import Arkham.I18n
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Window (getBatchId)

newtype WineCellarRuined = WineCellarRuined LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wineCellarRuined :: LocationCard WineCellarRuined
wineCellarRuined = location WineCellarRuined Cards.wineCellarRuined 3 (Static 0)

instance HasAbilities WineCellarRuined where
  getAbilities (WineCellarRuined a) =
    extendRevealed1 a
      $ scenarioI18n
      $ withI18nTooltip "wineCellarRuined.treacherousFooting"
      $ skillTestAbility
      $ forcedAbility a 1
      $ WouldMove #when You #any (be a) Anywhere

instance RunMessage WineCellarRuined where
  runMessage msg l@(WineCellarRuined attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getBatchId -> batchId) _ -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) (BatchTarget batchId) #agility (Fixed 2)
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      chooseOneM iid $ withI18n do
        labeled "takeOneDamage" $ assignDamage iid (attrs.ability 1) 1
        labeled "cancelMove" $ cancelMovement (attrs.ability 1) iid
      pure l
    _ -> WineCellarRuined <$> liftRunMessage msg attrs
