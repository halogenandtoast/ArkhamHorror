module Arkham.Asset.Cards.DarkHorse5 (
  darkHorse5,
  DarkHorse5 (..),
) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection
import Arkham.SkillType

newtype DarkHorse5 = DarkHorse5 AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

darkHorse5 :: AssetCard DarkHorse5
darkHorse5 = asset DarkHorse5 Cards.darkHorse5

instance HasModifiersFor DarkHorse5 where
  getModifiersFor (InvestigatorTarget iid) (DarkHorse5 a) | controlledBy a iid = do
    resourceCount <- field InvestigatorResources iid
    pure
      $ toModifiers
        a
        ( if resourceCount == 0
            then
              [ SkillModifier SkillWillpower 1
              , SkillModifier SkillIntellect 1
              , SkillModifier SkillCombat 1
              , SkillModifier SkillAgility 1
              , MayChooseNotToTakeUpkeepResources
              ]
            else [MayChooseNotToTakeUpkeepResources]
        )
  getModifiersFor _ _ = pure []

instance RunMessage DarkHorse5 where
  runMessage msg (DarkHorse5 attrs) = DarkHorse5 <$> runMessage msg attrs
