module Arkham.Homebrew.DarkMatter.Assets.DoctorFeng (doctorFeng) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Cards
import Arkham.Matcher

newtype DoctorFeng = DoctorFeng AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

doctorFeng :: AssetCard DoctorFeng
doctorFeng = ally DoctorFeng Cards.doctorFeng (2, 2)

{- | "Revelation - Put this card into play under your control.
[action] [action]: Heal 1 damage from each investigator and [[Ally]] asset at your
location. (Group limit once per game.)"
-}
instance HasAbilities DoctorFeng where
  getAbilities (DoctorFeng a) =
    [groupLimit PerGame $ controlled_ a 1 doubleActionAbility]

instance RunMessage DoctorFeng where
  runMessage msg a@(DoctorFeng attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      putCardIntoPlay iid attrs
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigators <- select $ colocatedWith iid
      for_ investigators \iid' -> healDamage iid' (attrs.ability 1) 1
      allies <- select $ #ally <> AssetAt (locationWithInvestigator iid)
      for_ allies \aid -> healDamage aid (attrs.ability 1) 1
      pure a
    _ -> DoctorFeng <$> liftRunMessage msg attrs
