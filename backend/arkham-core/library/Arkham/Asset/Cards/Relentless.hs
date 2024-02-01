module Arkham.Asset.Cards.Relentless (
  relentless,
  Relentless (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype Relentless = Relentless AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

relentless :: AssetCard Relentless
relentless = asset Relentless Cards.relentless

instance HasAbilities Relentless where
  getAbilities (Relentless a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility
          ( EnemyDealtExcessDamage Timing.When AnyDamageEffect AnyEnemy (SourceOwnedBy You)
          )
        $ ExhaustCost
        $ toTarget a
    , restrictedAbility a 2 (ControlsThis <> AnyDamageOnThis)
        $ FastAbility
        $ DiscardCost FromPlay
        $ toTarget a
    ]

toExcessDamage :: [Window] -> Int
toExcessDamage [] = error "invalid call"
toExcessDamage ((windowType -> Window.DealtExcessDamage _ _ _ n) : _) = n
toExcessDamage (_ : xs) = toExcessDamage xs

instance RunMessage Relentless where
  runMessage msg a@(Relentless attrs) = case msg of
    UseCardAbility _ source 1 windows' _ | isSource attrs source -> do
      let excessDamage = toExcessDamage windows'
      push $ PlaceDamage (toAbilitySource attrs 1) (toTarget attrs) excessDamage
      pure a
    UseCardAbility iid source 2 _ _ | isSource attrs source -> do
      push $ TakeResources iid (assetDamage attrs) (toAbilitySource attrs 2) False
      pure a
    _ -> Relentless <$> runMessage msg attrs
