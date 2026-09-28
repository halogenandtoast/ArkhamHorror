module Arkham.Homebrew.CircusExMortis.Assets.RichardStratton (richardStratton) where

import Arkham.Asset.Import.Lifted
import Arkham.Homebrew.CircusExMortis.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (Vice (Revelry))
import Arkham.Homebrew.CircusExMortis.Socialites

newtype RichardStratton = RichardStratton AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

richardStratton :: AssetCard RichardStratton
richardStratton = asset RichardStratton Cards.richardStratton

instance HasAbilities RichardStratton where
  getAbilities (RichardStratton a) = socialiteAbilities a

instance RunMessage RichardStratton where
  runMessage msg a@(RichardStratton attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      socialiteViceParley attrs iid #agility Revelry
      pure a
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n -> do
      socialiteSuccess attrs iid n
      pure a
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      assignDamage iid (attrs.ability 1) 1
      pure a
    _ -> RichardStratton <$> liftRunMessage msg attrs
