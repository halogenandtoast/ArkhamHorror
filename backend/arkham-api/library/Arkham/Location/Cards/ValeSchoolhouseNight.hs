module Arkham.Location.Cards.ValeSchoolhouseNight (valeSchoolhouseNight) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype ValeSchoolhouseNight = ValeSchoolhouseNight LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

valeSchoolhouseNight :: LocationCard ValeSchoolhouseNight
valeSchoolhouseNight = symbolLabel $ location ValeSchoolhouseNight Cards.valeSchoolhouseNight 0 (Static 0)

instance HasModifiersFor ValeSchoolhouseNight where
  getModifiersFor (ValeSchoolhouseNight attrs) =
    modifySelect attrs (EnemyAt $ be attrs) [EnemyEvade 1]

instance HasAbilities ValeSchoolhouseNight where
  getAbilities (ValeSchoolhouseNight a) =
    extendRevealed1 a $ groupLimit PerRound $ restricted a 1 Here actionAbility

instance RunMessage ValeSchoolhouseNight where
  runMessage msg l@(ValeSchoolhouseNight attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseBeginSkillTest sid iid (attrs.ability 1) iid [#willpower, #agility] (Fixed 3)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      investigators <- select $ InvestigatorAt (be attrs)
      chooseTargetM iid investigators \iid' -> chooseOneM iid do
        labeled "Heal 1 damage" $ healDamage iid' (attrs.ability 1) 1
        labeled "Heal 1 horror" $ healHorror iid' (attrs.ability 1) 1
      pure l
    _ -> ValeSchoolhouseNight <$> liftRunMessage msg attrs
