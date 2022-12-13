module Arkham.Location.Cards.PathOfThorns
  ( pathOfThorns
  , PathOfThorns(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing

newtype PathOfThorns = PathOfThorns LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pathOfThorns :: LocationCard PathOfThorns
pathOfThorns = location PathOfThorns Cards.pathOfThorns 3 (PerPlayer 1)

instance HasAbilities PathOfThorns where
  getAbilities (PathOfThorns a) = withBaseAbilities
    a
    [ mkAbility a 1 $ ForcedAbility $ SkillTestResult
      Timing.After
      You
      (WhileInvestigating $ LocationWithId $ toId a)
      (FailureResult AnyValue)
    , restrictedAbility a 2 Here
    $ ForcedAbility
    $ Explored Timing.After You
    $ FailedExplore AnyCard
    ]

instance RunMessage PathOfThorns where
  runMessage msg l@(PathOfThorns attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push $ InvestigatorAssignDamage iid source DamageAny 1 0
      pure l
    UseCardAbility iid source 2 _ _ | isSource attrs source -> do
      push $ InvestigatorAssignDamage iid source DamageAny 1 0
      pure l
    _ -> PathOfThorns <$> runMessage msg attrs
