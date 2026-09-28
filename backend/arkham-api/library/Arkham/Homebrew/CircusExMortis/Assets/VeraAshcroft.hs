module Arkham.Homebrew.CircusExMortis.Assets.VeraAshcroft (veraAshcroft) where

import Arkham.Asset.Import.Lifted
import Arkham.Homebrew.CircusExMortis.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (Vice (Intimacy))
import Arkham.Homebrew.CircusExMortis.Socialites

newtype VeraAshcroft = VeraAshcroft AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

veraAshcroft :: AssetCard VeraAshcroft
veraAshcroft = asset VeraAshcroft Cards.veraAshcroft

instance HasAbilities VeraAshcroft where
  getAbilities (VeraAshcroft a) = socialiteAbilities a

instance RunMessage VeraAshcroft where
  runMessage msg a@(VeraAshcroft attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      socialiteViceParley attrs iid #willpower Intimacy
      pure a
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n -> do
      socialiteSuccess attrs iid n
      pure a
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      assignHorror iid (attrs.ability 1) 1
      pure a
    _ -> VeraAshcroft <$> liftRunMessage msg attrs
