module Arkham.Asset.Cards.DarkHorse
  ( darkHorse
  , DarkHorse(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Investigator.Types ( Field(..) )
import Arkham.Projection
import Arkham.SkillType
import Arkham.Target

newtype DarkHorse = DarkHorse AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

darkHorse :: AssetCard DarkHorse
darkHorse = asset DarkHorse Cards.darkHorse

instance HasModifiersFor DarkHorse where
  getModifiersFor (InvestigatorTarget iid) (DarkHorse a) | controlledBy a iid = do
    resourceCount <- field InvestigatorResources iid
    pure $ toModifiers
      a
      (if resourceCount == 0
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

instance RunMessage DarkHorse where
  runMessage msg (DarkHorse attrs) = DarkHorse <$> runMessage msg attrs
