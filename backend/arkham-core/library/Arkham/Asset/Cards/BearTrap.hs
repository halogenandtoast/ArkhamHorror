module Arkham.Asset.Cards.BearTrap
  ( BearTrap(..)
  , bearTrap
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.LocationId
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Window (Window(..))
import Arkham.Window qualified as Window

newtype BearTrap = BearTrap AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bearTrap :: AssetCard BearTrap
bearTrap = assetWith BearTrap Cards.bearTrap (isStoryL .~ True)

instance HasModifiersFor BearTrap where
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

instance RunMessage BearTrap where
  runMessage msg a@(BearTrap attrs@AssetAttrs {..}) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      locationId <- getId @LocationId iid
      a <$ push (AttachAsset assetId (LocationTarget locationId))
    UseCardAbility _ source [Window _ (Window.EnemyEnters eid _)] 2 _
      | isSource attrs source -> do
        a <$ push (AttachAsset assetId (EnemyTarget eid))
    _ -> BearTrap <$> runMessage msg attrs
