module Arkham.Types.Asset.Cards.BearTrap
  ( BearTrap(..)
  , bearTrap
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.LocationId
import Arkham.Types.Matcher
import Arkham.Types.Modifier
import Arkham.Types.Target
import Arkham.Types.Timing qualified as Timing
import Arkham.Types.Window (Window(..))
import Arkham.Types.Window qualified as Window

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

instance HasAbilities BearTrap where
  getAbilities (BearTrap x) =
    [restrictedAbility x 1 restriction $ FastAbility Free]
      <> [ mkAbility x 2 $ ForcedAbility $ EnemyEnters
             Timing.After
             (LocationWithId attachedLocationId)
             (enemyIs Cards.theRougarou)
         | attachedLocationId <- maybeToList (assetLocation x)
         ]
    where restriction = maybe OwnsThis (const Never) (assetEnemy x)

instance AssetRunner env => RunMessage env BearTrap where
  runMessage msg a@(BearTrap attrs@AssetAttrs {..}) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      locationId <- getId @LocationId iid
      a <$ push (AttachAsset assetId (LocationTarget locationId))
    UseCardAbility _ source [Window _ (Window.EnemyEnters eid _)] 2 _
      | isSource attrs source -> do
        a <$ push (AttachAsset assetId (EnemyTarget eid))
    _ -> BearTrap <$> runMessage msg attrs
