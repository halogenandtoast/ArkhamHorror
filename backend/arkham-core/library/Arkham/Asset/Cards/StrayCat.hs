module Arkham.Asset.Cards.StrayCat
  ( StrayCat(..)
  , strayCat
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher hiding ( EnemyEvaded )

newtype StrayCat = StrayCat AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

strayCat :: AssetCard StrayCat
strayCat = ally StrayCat Cards.strayCat (1, 0)

instance HasAbilities StrayCat where
  getAbilities (StrayCat a) =
    [ restrictedAbility
          a
          1
          (ControlsThis <> EnemyCriteria (EnemyExists $ EnemyAt YourLocation))
        $ FastAbility
        $ DiscardCost FromPlay
        $ toTarget a
    ]

instance RunMessage StrayCat where
  runMessage msg a@(StrayCat attrs) = case msg of
    InDiscard _ (UseCardAbility iid source 1 _ _) | isSource attrs source -> do
      enemies <-
        selectList
        $ EnemyAt (LocationWithInvestigator $ InvestigatorWithId iid)
        <> NonEliteEnemy

      push $ chooseOne
        iid
        [ targetLabel enemyId [EnemyEvaded iid enemyId] | enemyId <- enemies ]
      pure a
    _ -> StrayCat <$> runMessage msg attrs
