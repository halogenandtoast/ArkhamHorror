module Arkham.Types.Asset.Cards.BearTrap
  ( BearTrap(..)
  , bearTrap
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Restriction
import Arkham.Types.Target

newtype BearTrap = BearTrap AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, Generic, ToJSON, FromJSON, Entity)

bearTrap :: AssetCard BearTrap
bearTrap = assetWith BearTrap Cards.bearTrap (isStoryL .~ True)

instance HasModifiersFor env BearTrap where
  getModifiersFor _ (EnemyTarget eid) (BearTrap attrs@AssetAttrs {..})
    | Just eid == assetEnemy = pure
    $ toModifiers attrs [EnemyFight (-1), EnemyEvade (-1)]
  getModifiersFor _ _ _ = pure []

instance HasActions BearTrap where
  getActions (BearTrap x) =
    [restrictedAbility x 1 restriction $ FastAbility Free]
    where restriction = maybe OwnsThis (const Never) (assetEnemy x)

instance AssetRunner env => RunMessage env BearTrap where
  runMessage msg a@(BearTrap attrs@AssetAttrs {..}) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      locationId <- getId @LocationId iid
      a <$ push (AttachAsset assetId (LocationTarget locationId))
    EnemyMove eid _ lid | Just lid == assetLocation -> do
      isRougarou <- (== CardCode "81028") <$> getId eid
      a <$ when isRougarou (push (AttachAsset assetId (EnemyTarget eid)))
    _ -> BearTrap <$> runMessage msg attrs
