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
    [ withCriteria (mkAbility a 1 $ FastAbility $ DiscardCost FromPlay (toTarget a))
        $ ControlsThis <> EnemyCriteria (EnemyExists $ EnemyAt YourLocation)
    ]

instance RunMessage StrayCat where
  runMessage msg a@(StrayCat attrs) = case msg of
    InDiscard _ (UseCardAbility iid (isSource attrs -> True) 1 _ _) -> do
      enemies <- selectList $ EnemyAt (locationWithInvestigator iid) <> NonEliteEnemy

      push
        $ chooseOne iid
        $ [targetLabel enemy [EnemyEvaded iid enemy] | enemy <- enemies]
      pure a
    _ -> StrayCat <$> runMessage msg attrs
