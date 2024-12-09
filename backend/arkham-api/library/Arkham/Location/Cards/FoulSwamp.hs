module Arkham.Location.Cards.FoulSwamp (FoulSwamp (..), foulSwamp) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Location.Cards qualified as Cards (foulSwamp)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.ScenarioLogKey

newtype FoulSwamp = FoulSwamp LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

foulSwamp :: LocationCard FoulSwamp
foulSwamp = location FoulSwamp Cards.foulSwamp 2 (Static 0)

instance HasModifiersFor FoulSwamp where
  getModifiersFor (FoulSwamp attrs) =
    modifySelect attrs (investigatorAt attrs) [CannotHealHorror, CannotCancelHorror]

instance HasAbilities FoulSwamp where
  getAbilities (FoulSwamp attrs) =
    extendRevealed1 attrs
      $ skillTestAbility
      $ restricted attrs 1 Here
      $ actionAbilityWithCost (UpTo (Fixed 3) (HorrorCost (toSource attrs) YouTarget 1))

instance RunMessage FoulSwamp where
  runMessage msg l@(FoulSwamp attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ payments -> do
      let
        horrorPayment = \case
          Payments ps -> foldMap horrorPayment ps
          HorrorPayment a -> Sum a
          _ -> Sum 0
        n = getSum $ horrorPayment payments
        source = toAbilitySource attrs 1

      sid <- getRandom
      skillTestModifier sid source iid $ SkillModifier #willpower n
      beginSkillTest sid iid (attrs.ability 1) attrs #willpower (Fixed 7)
      pure l
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      remember FoundAnAncientBindingStone
      pure l
    _ -> FoulSwamp <$> liftRunMessage msg attrs
