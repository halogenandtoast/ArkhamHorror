module Arkham.Asset.Cards.Relentless
  ( relentless
  , Relentless(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import qualified Arkham.Asset.Cards as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.Timing qualified as Timing
import Arkham.Window (Window(..))
import Arkham.Window qualified as Window

newtype Relentless = Relentless AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

relentless :: AssetCard Relentless
relentless = asset Relentless Cards.relentless

instance HasAbilities Relentless where
  getAbilities (Relentless a) =
    [ restrictedAbility a 1 ControlsThis
      $ ReactionAbility
          (EnemyDealtExcessDamage Timing.When AnyDamageEffect AnyEnemy AnySource
          )
      $ ExhaustCost
      $ toTarget a
    , restrictedAbility a 2 ControlsThis $ FastAbility $ DiscardCost $ toTarget
      a
    ]

toExcessDamage :: [Window] -> Int
toExcessDamage [] = error "invalid call"
toExcessDamage (Window _ (Window.DealtExcessDamage _ _ _ n) : _) = n
toExcessDamage (_ : xs) = toExcessDamage xs

instance RunMessage Relentless where
  runMessage msg a@(Relentless attrs) = case msg of
    UseCardAbility _ source windows' 1 _ | isSource attrs source -> do
      let excessDamage = toExcessDamage windows'
      push $ PlaceDamage (toTarget attrs) excessDamage
      pure a
    UseCardAbility iid source _ 2 _ | isSource attrs source -> do
      push $ TakeResources iid (assetDamage attrs) False
      pure a
    _ -> Relentless <$> runMessage msg attrs
