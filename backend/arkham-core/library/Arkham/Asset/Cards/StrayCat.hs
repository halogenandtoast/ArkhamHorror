module Arkham.Asset.Cards.StrayCat (
  StrayCat (..),
  strayCat,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher hiding (EnemyEvaded)

newtype StrayCat = StrayCat AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

strayCat :: AssetCard StrayCat
strayCat = ally StrayCat Cards.strayCat (1, 0)

instance HasAbilities StrayCat where
  getAbilities (StrayCat a) =
    [ controlledAbility a 1 (ControlsThis <> exists (EnemyAt YourLocation))
        $ FastAbility
        $ DiscardCost FromPlay (toTarget a)
    ]

instance RunMessage StrayCat where
  runMessage msg a@(StrayCat attrs) = case msg of
    InDiscard _ (UseThisAbility iid (isSource attrs -> True) 1) -> do
      enemies <- selectList $ enemyAtLocationWith iid <> NonEliteEnemy
      push $ chooseOne iid $ targetLabels enemies (only . EnemyEvaded iid)
      pure a
    _ -> StrayCat <$> runMessage msg attrs
