module Arkham.Types.Act.Cards.UncoveringTheConspiracy
  ( UncoveringTheConspiracy(..)
  , uncoveringTheConspiracy
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
import Arkham.Types.Classes
import Arkham.Types.Criteria
import Arkham.Types.GameValue
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Resolution
import Arkham.Types.Source
import Arkham.Types.Trait

newtype UncoveringTheConspiracy = UncoveringTheConspiracy ActAttrs
  deriving anyclass (IsAct, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

uncoveringTheConspiracy :: ActCard UncoveringTheConspiracy
uncoveringTheConspiracy =
  act (1, A) UncoveringTheConspiracy Cards.uncoveringTheConspiracy Nothing

instance HasAbilities env UncoveringTheConspiracy where
  getAbilities _ _ (UncoveringTheConspiracy a) = pure
    [ mkAbility a 1
    $ ActionAbility Nothing
    $ ActionCost 1
    <> GroupClueCost (PerPlayer 2) Anywhere
    , restrictedAbility
      a
      2
      (InVictoryDisplay
        (CardWithTrait Cultist <> CardIsUnique)
        (EqualTo $ Static 6)
      )
    $ Objective
    $ ForcedAbility AnyWindow
    ]

instance ActRunner env => RunMessage env UncoveringTheConspiracy where
  runMessage msg a@(UncoveringTheConspiracy attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      a <$ push (UseScenarioSpecificAbility iid Nothing 1)
    UseCardAbility iid source _ 2 _ | isSource attrs source ->
      a <$ push (AdvanceAct (toId attrs) (InvestigatorSource iid))
    AdvanceAct aid _ | aid == toId attrs && onSide B attrs ->
      a <$ push (ScenarioResolution $ Resolution 1)
    _ -> UncoveringTheConspiracy <$> runMessage msg attrs
