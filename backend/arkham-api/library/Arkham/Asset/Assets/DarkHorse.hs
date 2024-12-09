module Arkham.Asset.Assets.DarkHorse (darkHorse, DarkHorse (..)) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Investigator.Types (Field (..))
import Arkham.Prelude
import Arkham.Projection

newtype DarkHorse = DarkHorse AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

darkHorse :: AssetCard DarkHorse
darkHorse = asset DarkHorse Cards.darkHorse

instance HasModifiersFor DarkHorse where
  getModifiersFor (DarkHorse a) = case a.controller of
    Just iid -> do
      resourceCount <- field InvestigatorResources iid
      modified_ a iid
        $ if resourceCount == 0
          then
            [ SkillModifier #willpower 1
            , SkillModifier #intellect 1
            , SkillModifier #combat 1
            , SkillModifier #agility 1
            , MayChooseNotToTakeUpkeepResources
            ]
          else [MayChooseNotToTakeUpkeepResources]
    Nothing -> pure mempty

instance RunMessage DarkHorse where
  runMessage msg (DarkHorse attrs) = DarkHorse <$> runMessage msg attrs
