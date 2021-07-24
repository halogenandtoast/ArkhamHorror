module Arkham.Types.Asset.Cards.StrayCat
  ( StrayCat(..)
  , strayCat
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Trait
import Arkham.Types.Window

newtype StrayCat = StrayCat AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

strayCat :: AssetCard StrayCat
strayCat = ally StrayCat Cards.strayCat (1, 0)

instance HasModifiersFor env StrayCat

ability :: AssetAttrs -> Ability
ability a = mkAbility (toSource a) 1 (FastAbility (DiscardCost $ toTarget a))

instance HasActions env StrayCat where
  getActions iid FastPlayerWindow (StrayCat a) | ownedBy a iid =
    withBaseActions iid FastPlayerWindow a $ pure [UseAbility iid (ability a)]
  getActions _ _ _ = pure []

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
