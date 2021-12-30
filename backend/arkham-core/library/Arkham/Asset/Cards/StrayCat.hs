module Arkham.Asset.Cards.StrayCat
  ( StrayCat(..)
  , strayCat
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Attrs
import Arkham.Cost
import Arkham.Criteria
import Arkham.Id
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Target

newtype StrayCat = StrayCat AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

strayCat :: AssetCard StrayCat
strayCat = ally StrayCat Cards.strayCat (1, 0)

instance HasAbilities StrayCat where
  getAbilities (StrayCat a) =
    [ restrictedAbility
          a
          1
          (OwnsThis <> EnemyCriteria (EnemyExists $ EnemyAt YourLocation))
        $ FastAbility
        $ DiscardCost
        $ toTarget a
    ]

instance AssetRunner env => RunMessage env StrayCat where
  runMessage msg a@(StrayCat attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      locationId <- getId @LocationId iid
      enemies <- selectList
        (EnemyAt (LocationWithId locationId) <> NonEliteEnemy)

      a <$ push
        (chooseOne
          iid
          [ TargetLabel (EnemyTarget enemyId) [EnemyEvaded iid enemyId]
          | enemyId <- enemies
          ]
        )
    _ -> StrayCat <$> runMessage msg attrs
