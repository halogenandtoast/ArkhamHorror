module Arkham.Location.Cards.ArkhamWoodsHiddenPath (arkhamWoodsHiddenPath) where

import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectWhen)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Phase
import Arkham.Trait (Trait (Witch))

newtype ArkhamWoodsHiddenPath = ArkhamWoodsHiddenPath LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

arkhamWoodsHiddenPath :: LocationCard ArkhamWoodsHiddenPath
arkhamWoodsHiddenPath = location ArkhamWoodsHiddenPath Cards.arkhamWoodsHiddenPath 2 (PerPlayer 1)

instance HasModifiersFor ArkhamWoodsHiddenPath where
  getModifiersFor (ArkhamWoodsHiddenPath a) = do
    mEnemyPhaseStep <- getEnemyPhaseStep
    modifySelectWhen
      a
      (mEnemyPhaseStep == Just ResolveAttacksStep)
      (at_ (be a) <> ReadyEnemy <> withTrait Witch)
      [EnemyAttacksOverride Anyone, DoNotExhaust]

instance RunMessage ArkhamWoodsHiddenPath where
  runMessage msg (ArkhamWoodsHiddenPath attrs) =
    ArkhamWoodsHiddenPath <$> runMessage msg attrs
