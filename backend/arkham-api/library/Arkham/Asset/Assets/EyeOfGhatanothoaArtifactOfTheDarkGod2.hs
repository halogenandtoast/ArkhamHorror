module Arkham.Asset.Assets.EyeOfGhatanothoaArtifactOfTheDarkGod2 (eyeOfGhatanothoaArtifactOfTheDarkGod2) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)

newtype EyeOfGhatanothoaArtifactOfTheDarkGod2 = EyeOfGhatanothoaArtifactOfTheDarkGod2 AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eyeOfGhatanothoaArtifactOfTheDarkGod2 :: AssetCard EyeOfGhatanothoaArtifactOfTheDarkGod2
eyeOfGhatanothoaArtifactOfTheDarkGod2 =
  assetWith
    EyeOfGhatanothoaArtifactOfTheDarkGod2
    Cards.eyeOfGhatanothoaArtifactOfTheDarkGod2
    (healthL ?~ 3)

instance HasModifiersFor EyeOfGhatanothoaArtifactOfTheDarkGod2 where
  getModifiersFor (EyeOfGhatanothoaArtifactOfTheDarkGod2 a) = do
    controllerGets a [SkillModifier #willpower 1, TopCardOfEncounterDeckIsRevealed]

instance RunMessage EyeOfGhatanothoaArtifactOfTheDarkGod2 where
  runMessage msg (EyeOfGhatanothoaArtifactOfTheDarkGod2 attrs) =
    EyeOfGhatanothoaArtifactOfTheDarkGod2 <$> runMessage msg attrs
