module Arkham.Asset.Cards.BearTrap
  ( BearTrap(..)
  , bearTrap
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Matcher
import Arkham.Placement
import Arkham.Projection
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Window ( Window (..) )
import Arkham.Window qualified as Window

newtype BearTrap = BearTrap AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bearTrap :: AssetCard BearTrap
bearTrap = assetWith BearTrap Cards.bearTrap (isStoryL .~ True)

instance HasModifiersFor BearTrap where
  getModifiersFor (EnemyTarget eid) (BearTrap attrs@AssetAttrs {..}) =
    pure . toModifiers attrs $ case assetPlacement of
      AttachedToEnemy eid' | eid == eid' -> [EnemyFight (-1), EnemyEvade (-1)]
      _ -> []
  getModifiersFor _ _ = pure []

instance HasAbilities BearTrap where
  getAbilities (BearTrap x) =
    [ restrictedAbility x 1 restriction $ FastAbility Free
    , mkAbility x 2 $ ForcedAbility $ EnemyEnters
      Timing.After
      LocationOfThis
      (enemyIs Cards.theRougarou)
    ]
   where
    restriction = case assetPlacement x of
      AttachedToEnemy _ -> Never
      _ -> ControlsThis

instance RunMessage BearTrap where
  runMessage msg a@(BearTrap attrs@AssetAttrs {..}) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      locationId <- fieldMap
        InvestigatorLocation
        (fromJustNote "must be at a location")
        iid
      a <$ push (AttachAsset assetId (LocationTarget locationId))
    UseCardAbility _ source 2 [Window _ (Window.EnemyEnters eid _)] _
      | isSource attrs source -> do
        a <$ push (AttachAsset assetId (EnemyTarget eid))
    _ -> BearTrap <$> runMessage msg attrs
