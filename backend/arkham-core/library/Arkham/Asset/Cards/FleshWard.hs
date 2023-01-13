module Arkham.Asset.Cards.FleshWard
  ( fleshWard
  , FleshWard(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.Timing qualified as Timing
import Arkham.Window ( Window (..) )
import Arkham.Window qualified as Window

newtype FleshWard = FleshWard AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fleshWard :: AssetCard FleshWard
fleshWard = asset FleshWard Cards.fleshWard

instance HasAbilities FleshWard where
  getAbilities (FleshWard a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility
            (DealtDamageOrHorror Timing.When (SourceIsEnemyAttack AnyEnemy) You)
        $ ExhaustCost (toTarget a)
        <> UseCost (AssetWithId $ toId a) Charge 1
    ]

dealtDamage :: [Window] -> Bool
dealtDamage [] = False
dealtDamage (Window _ (Window.WouldTakeDamageOrHorror _ _ n _) : _) = n > 0
dealtDamage (_ : xs) = dealtDamage xs

dealtHorror :: [Window] -> Bool
dealtHorror [] = False
dealtHorror (Window _ (Window.WouldTakeDamageOrHorror _ _ _ n) : _) = n > 0
dealtHorror (_ : xs) = dealtDamage xs

instance RunMessage FleshWard where
  runMessage msg a@(FleshWard attrs) = case msg of
    UseCardAbility iid source 1 windows' _ | isSource attrs source -> do
      ignoreWindow <- checkWindows [Window Timing.After (Window.CancelledOrIgnoredCardOrGameEffect $ toAbilitySource attrs 1)]
      push
        $ chooseOrRunOne iid
        $ [ Label "Cancel 1 damage" [CancelDamage iid 1, ignoreWindow]
          | dealtDamage windows'
          ]
        <> [ Label "Cancel 1 horror" [CancelHorror iid 1, ignoreWindow]
           | dealtHorror windows'
           ]
      pure a
    _ -> FleshWard <$> runMessage msg attrs
