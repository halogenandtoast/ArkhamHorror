module Arkham.Homebrew.CircusExMortis.Assets.EstherMeredith (estherMeredith) where

import Arkham.Asset.Import.Lifted
import Arkham.Homebrew.CircusExMortis.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (Vice (Opulence))
import Arkham.Homebrew.CircusExMortis.Socialites

newtype EstherMeredith = EstherMeredith AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

estherMeredith :: AssetCard EstherMeredith
estherMeredith = asset EstherMeredith Cards.estherMeredith

instance HasAbilities EstherMeredith where
  getAbilities (EstherMeredith a) = socialiteAbilities a

instance RunMessage EstherMeredith where
  runMessage msg a@(EstherMeredith attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      socialiteViceParley attrs iid #intellect Opulence
      pure a
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n -> do
      socialiteSuccess attrs iid n
      pure a
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      assignHorror iid (attrs.ability 1) 1
      pure a
    _ -> EstherMeredith <$> liftRunMessage msg attrs
