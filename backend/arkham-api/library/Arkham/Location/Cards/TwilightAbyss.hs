module Arkham.Location.Cards.TwilightAbyss (twilightAbyss) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted.Choose

newtype TwilightAbyss = TwilightAbyss LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

twilightAbyss :: LocationCard TwilightAbyss
twilightAbyss = location TwilightAbyss Cards.twilightAbyss 2 (PerPlayer 2)

instance HasAbilities TwilightAbyss where
  getAbilities (TwilightAbyss a) =
    extendRevealed1 a $ skillTestAbility $ mkAbility a 1 $ forced $ Enters #after You (be a)

instance RunMessage TwilightAbyss where
  runMessage msg l@(TwilightAbyss attrs) = runQueueT $ case msg of
    Msg.RevealLocation _ lid | lid == toId attrs -> do
      TwilightAbyss <$> liftRunMessage msg (attrs & labelL .~ "twilightAbyss")
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseBeginSkillTest sid iid (attrs.ability 1) iid [#combat, #agility] (Fixed 3)
      pure l
    FailedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n -> do
      assignDamage iid (attrs.ability 1) n
      pure l
    _ -> TwilightAbyss <$> liftRunMessage msg attrs
