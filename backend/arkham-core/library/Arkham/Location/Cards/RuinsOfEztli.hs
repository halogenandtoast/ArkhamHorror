module Arkham.Location.Cards.RuinsOfEztli
  ( ruinsOfEztli
  , RuinsOfEztli(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing

newtype RuinsOfEztli = RuinsOfEztli LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ruinsOfEztli :: LocationCard RuinsOfEztli
ruinsOfEztli = location RuinsOfEztli Cards.ruinsOfEztli 3 (PerPlayer 2)

instance HasAbilities RuinsOfEztli where
  getAbilities (RuinsOfEztli attrs) = withBaseAbilities
    attrs
    [ mkAbility attrs 1
      $ ForcedAbility
      $ SkillTestResult
          Timing.After
          You
          (WhileInvestigating $ LocationWithId $ toId attrs)
      $ FailureResult AnyValue
    ]

instance RunMessage RuinsOfEztli where
  runMessage msg l@(RuinsOfEztli attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      push $ InvestigatorDrawEncounterCard iid
      pure l
    _ -> RuinsOfEztli <$> runMessage msg attrs
