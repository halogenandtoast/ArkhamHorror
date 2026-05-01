module Arkham.Location.Cards.UptownStMarysHospital (uptownStMarysHospital) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards (uptownStMarysHospital)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype UptownStMarysHospital = UptownStMarysHospital LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

uptownStMarysHospital :: LocationCard UptownStMarysHospital
uptownStMarysHospital = location UptownStMarysHospital Cards.uptownStMarysHospital 2 (PerPlayer 2)

instance HasAbilities UptownStMarysHospital where
  getAbilities (UptownStMarysHospital a) =
    extendRevealed1 a
      $ playerLimit PerGame
      $ restricted
        a
        1
        ( Here
            <> oneOf
              [ exists $ HealableInvestigator (a.ability 1) #damage $ investigatorAt a
              , exists $ HealableAsset (a.ability 1) #damage $ #ally <> assetAt a
              ]
        )
        actionAbility

instance RunMessage UptownStMarysHospital where
  runMessage msg l@(UptownStMarysHospital attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigators <- select $ HealableInvestigator (attrs.ability 1) #damage $ investigatorAt attrs
      allies <- select $ HealableAsset (attrs.ability 1) #damage $ #ally <> assetAt attrs
      withI18n $ chooseUpToNM' iid 2 "done" do
        targets investigators $ healDamageOn (attrs.ability 1) 1
        targets allies $ healDamageOn (attrs.ability 1) 1
      pure l
    _ -> UptownStMarysHospital <$> liftRunMessage msg attrs
