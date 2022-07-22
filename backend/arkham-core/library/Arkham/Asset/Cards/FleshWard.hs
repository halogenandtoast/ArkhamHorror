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
import Arkham.Window (Window(..))
import Arkham.Window qualified as Window

newtype FleshWard = FleshWard AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fleshWard :: AssetCard FleshWard
fleshWard = asset FleshWard Cards.fleshWard

instance HasAbilities FleshWard where
  getAbilities (FleshWard a) =
    [ restrictedAbility a 1 ControlsThis $ ReactionAbility
        (OrWindowMatcher
          [ DealtDamage Timing.When (SourceIsEnemyAttack AnyEnemy) You
          , DealtHorror Timing.When (SourceIsEnemyAttack AnyEnemy) You
          ]
        )
        (ExhaustCost (toTarget a) <> UseCost (AssetWithId $ toId a) Charge 1)
    ]

dealtDamage :: [Window] -> Bool
dealtDamage [] = False
dealtDamage (Window _ (Window.DealtDamage{}) : _) = True
dealtDamage (_ : xs) = dealtDamage xs

dealtHorror :: [Window] -> Bool
dealtHorror [] = False
dealtHorror (Window _ (Window.DealtHorror{}) : _) = True
dealtHorror (_ : xs) = dealtDamage xs

instance RunMessage FleshWard where
  runMessage msg a@(FleshWard attrs) = case msg of
    UseCardAbility iid source windows' 1 _ | isSource attrs source -> do
      push
        $ chooseOrRunOne iid
        $ [ Label "Cancel 1 damage" [CancelDamage iid 1]
          | dealtDamage windows'
          ]
        <> [ Label "Cancel 1 horror" [CancelHorror iid 1]
           | dealtHorror windows'
           ]
      pure a
    _ -> FleshWard <$> runMessage msg attrs
