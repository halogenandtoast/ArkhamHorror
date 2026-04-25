module Arkham.Location.Cards.UptownStMarrysHospital (uptownStMarrysHospital) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (uptownStMarrysHospital)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype UptownStMarrysHospital = UptownStMarrysHospital LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

uptownStMarrysHospital :: LocationCard UptownStMarrysHospital
uptownStMarrysHospital = location UptownStMarrysHospital Cards.uptownStMarrysHospital 2 (PerPlayer 2)

instance HasAbilities UptownStMarrysHospital where
  getAbilities (UptownStMarrysHospital a) =
    extendRevealed1 a
      $ playerLimit PerGame
      $ restricted a 1 (Here <> exists (HealableInvestigator (a.ability 1) #damage You)) actionAbility

instance RunMessage UptownStMarrysHospital where
  runMessage msg l@(UptownStMarrysHospital attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigators <- select (investigatorAt attrs)
      allies <- select (AllyAsset <> assetAt attrs)
      chooseUpToNM iid 2 "Heal up to 2 damage, divided as you choose" do
        targets investigators \iid' -> healDamage iid' (attrs.ability 1) 1
        targets allies \aid -> healDamage aid (attrs.ability 1) 1
      pure l
    _ -> UptownStMarrysHospital <$> liftRunMessage msg attrs
