module Arkham.Location.Cards.CasinoLoungeCalmNight (casinoLoungeCalmNight) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.ScenarioLogKey

newtype CasinoLoungeCalmNight = CasinoLoungeCalmNight LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

casinoLoungeCalmNight :: LocationCard CasinoLoungeCalmNight
casinoLoungeCalmNight = symbolLabel $ location CasinoLoungeCalmNight Cards.casinoLoungeCalmNight 5 (PerPlayer 2)

instance HasAbilities CasinoLoungeCalmNight where
  getAbilities (CasinoLoungeCalmNight a) =
    extendRevealed1 a
      $ skillTestAbility
      $ restricted a 1 (Here <> thisExists a LocationWithoutClues) actionAbility

instance RunMessage CasinoLoungeCalmNight where
  runMessage msg l@(CasinoLoungeCalmNight attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #agility (Fixed 3)
      pure l
    PassedThisSkillTest _iid (isAbilitySource attrs 1 -> True) -> do
      remember FoundAVent
      pure l
    _ -> CasinoLoungeCalmNight <$> liftRunMessage msg attrs
