module Arkham.Types.Asset.Cards.DarkHorse
  ( darkHorse
  , DarkHorse(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Types.Asset.Attrs
import Arkham.Types.Id
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype DarkHorse = DarkHorse AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

darkHorse :: AssetCard DarkHorse
darkHorse = asset DarkHorse Cards.darkHorse

instance HasCount ResourceCount env InvestigatorId => HasModifiersFor env DarkHorse where
  getModifiersFor _ (InvestigatorTarget iid) (DarkHorse a) | ownedBy a iid = do
    resourceCount <- unResourceCount <$> getCount iid
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

  getModifiersFor _ _ _ = pure []

instance AssetRunner env => RunMessage env DarkHorse where
  runMessage msg (DarkHorse attrs) = DarkHorse <$> runMessage msg attrs
