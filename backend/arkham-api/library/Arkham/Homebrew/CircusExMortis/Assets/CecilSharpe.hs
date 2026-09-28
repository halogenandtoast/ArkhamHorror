module Arkham.Homebrew.CircusExMortis.Assets.CecilSharpe (cecilSharpe) where

import Arkham.Asset.Import.Lifted
import Arkham.Homebrew.CircusExMortis.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (Vice (Violence))
import Arkham.Homebrew.CircusExMortis.Socialites

newtype CecilSharpe = CecilSharpe AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cecilSharpe :: AssetCard CecilSharpe
cecilSharpe = asset CecilSharpe Cards.cecilSharpe

instance HasAbilities CecilSharpe where
  getAbilities (CecilSharpe a) = socialiteAbilities a

instance RunMessage CecilSharpe where
  runMessage msg a@(CecilSharpe attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      socialiteViceParley attrs iid #combat Violence
      pure a
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n -> do
      socialiteSuccess attrs iid n
      pure a
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      assignDamage iid (attrs.ability 1) 1
      pure a
    _ -> CecilSharpe <$> liftRunMessage msg attrs
