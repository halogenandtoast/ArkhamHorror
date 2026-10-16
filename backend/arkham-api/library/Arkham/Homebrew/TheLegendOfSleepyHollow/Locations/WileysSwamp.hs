module Arkham.Homebrew.TheLegendOfSleepyHollow.Locations.WileysSwamp (wileysSwamp) where

import Arkham.Ability
import Arkham.Homebrew.TheLegendOfSleepyHollow.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.TheLegendOfSleepyHollow.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (Field (LocationDoom))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log (remember)

newtype WileysSwamp = WileysSwamp LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wileysSwamp :: LocationCard WileysSwamp
wileysSwamp = location WileysSwamp Cards.wileysSwamp 3 (PerPlayer 2)

instance HasAbilities WileysSwamp where
  getAbilities (WileysSwamp a) =
    extendRevealed
      a
      [ scenarioI18n $ hauntedI "wileysSwamp.haunted" a 1
      , scenarioI18n
          $ withI18nTooltip "wileysSwamp.path"
          $ skillTestAbility
          $ restricted a 2 Here actionAbility
      ]

instance RunMessage WileysSwamp where
  runMessage msg l@(WileysSwamp attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      loseResources iid (attrs.ability 1) 1
      n <- amountFailedBy
      when (n >= 2) $ assignHorror iid (attrs.ability 1) 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      doom <- getSum <$> selectAgg Sum LocationDoom Anywhere
      sid <- getRandom
      chooseBeginSkillTest
        sid
        iid
        (attrs.ability 2)
        attrs
        [#intellect, #willpower]
        (Fixed $ max 0 (6 - doom))
      pure l
    PassedThisSkillTest _ (isAbilitySource attrs 2 -> True) -> do
      remember studiedTheHorsemansPath
      pure l
    _ -> WileysSwamp <$> liftRunMessage msg attrs
