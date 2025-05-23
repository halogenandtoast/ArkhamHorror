module Arkham.Asset.Assets.DarkHorse (darkHorse) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection

newtype DarkHorse = DarkHorse AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

darkHorse :: AssetCard DarkHorse
darkHorse = asset DarkHorse Cards.darkHorse

instance HasModifiersFor DarkHorse where
  getModifiersFor (DarkHorse a) = for_ a.controller \iid -> do
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

instance RunMessage DarkHorse where
  runMessage msg (DarkHorse attrs) = DarkHorse <$> runMessage msg attrs
