module Arkham.Asset.Assets.DarkHorse5 (darkHorse5) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Prelude
import Arkham.Projection

newtype DarkHorse5 = DarkHorse5 AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

darkHorse5 :: AssetCard DarkHorse5
darkHorse5 = asset DarkHorse5 Cards.darkHorse5

instance HasModifiersFor DarkHorse5 where
  getModifiersFor (DarkHorse5 a) = for_ a.controller \iid -> do
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

instance RunMessage DarkHorse5 where
  runMessage msg (DarkHorse5 attrs) = DarkHorse5 <$> runMessage msg attrs
