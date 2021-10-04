module Arkham.Types.Asset.Cards.StrayCat
  ( StrayCat(..)
  , strayCat
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Id
import Arkham.Types.Trait

newtype StrayCat = StrayCat AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

strayCat :: AssetCard StrayCat
strayCat = ally StrayCat Cards.strayCat (1, 0)

instance HasAbilities StrayCat where
  getAbilities (StrayCat a) =
    [restrictedAbility a 1 OwnsThis $ FastAbility $ DiscardCost $ toTarget a]

instance AssetRunner env => RunMessage env StrayCat where
  runMessage msg a@(StrayCat attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      locationId <- getId @LocationId iid
      locationEnemyIds <- getSetList locationId
      nonEliteEnemyIds <- filterM
        ((notMember Elite <$>) . getSet)
        locationEnemyIds

      a
        <$ push
             (chooseOne
               iid
               [ EnemyEvaded iid enemyId | enemyId <- nonEliteEnemyIds ]
             )
    _ -> StrayCat <$> runMessage msg attrs
