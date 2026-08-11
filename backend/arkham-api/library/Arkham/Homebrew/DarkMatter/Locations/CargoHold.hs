module Arkham.Homebrew.DarkMatter.Locations.CargoHold (cargoHold) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen)
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype CargoHold = CargoHold LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cargoHold :: LocationCard CargoHold
cargoHold = location CargoHold Cards.cargoHold 2 (Static 1)

{- | Unrevealed: "The pressure door leading to the back of the ship needs to be
rewired. As an additional cost to enter the Cargo Hold, investigators in the
Mess Hall must spend 1 clue, as a group."
-}
instance HasModifiersFor CargoHold where
  getModifiersFor (CargoHold a) =
    modifySelfWhen
      a
      (not a.revealed)
      [AdditionalCostToEnter $ GroupClueCost (Static 1) (locationIs Cards.messHall)]

instance HasAbilities CargoHold where
  getAbilities (CargoHold a) =
    extendRevealed1 a $ skillTestAbility $ forcedAbility a 1 $ Enters #after You (be a)

instance RunMessage CargoHold where
  runMessage msg l@(CargoHold attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 2)
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      assignHorror iid (attrs.ability 1) 1
      pure l
    _ -> CargoHold <$> liftRunMessage msg attrs
