module Arkham.Homebrew.DarkMatter.Locations.VentilationShaft (ventilationShaft) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Window (getBatchId)

newtype VentilationShaft = VentilationShaft LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ventilationShaft :: LocationCard VentilationShaft
ventilationShaft = symbolLabel $ location VentilationShaft Cards.ventilationShaft 2 (PerPlayer 1)

instance HasAbilities VentilationShaft where
  getAbilities (VentilationShaft a) =
    extendRevealed1 a
      $ skillTestAbility
      $ forcedAbility a 1
      $ WouldMove #when You #any Anywhere (be a)

instance RunMessage VentilationShaft where
  runMessage msg l@(VentilationShaft attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getBatchId -> batchId) _ -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) (BatchTarget batchId) #agility (Fixed 3)
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      cancelMovement (attrs.ability 1) iid
      pure l
    _ -> VentilationShaft <$> liftRunMessage msg attrs
