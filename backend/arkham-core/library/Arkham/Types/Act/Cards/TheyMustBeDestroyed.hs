module Arkham.Types.Act.Cards.TheyMustBeDestroyed
  ( TheyMustBeDestroyed(..)
  , theyMustBeDestroyed
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
import Arkham.Types.Classes
import Arkham.Types.Criteria
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Resolution

newtype TheyMustBeDestroyed = TheyMustBeDestroyed ActAttrs
  deriving anyclass (IsAct, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theyMustBeDestroyed :: ActCard TheyMustBeDestroyed
theyMustBeDestroyed =
  act (2, A) TheyMustBeDestroyed Cards.theyMustBeDestroyed Nothing

instance HasAbilities TheyMustBeDestroyed where
  getAbilities (TheyMustBeDestroyed x) =
    [ restrictedAbility
          x
          1
          (Negate $ AnyCriterion
            [ EnemyCriteria $ EnemyExists $ EnemyWithTitle
              "Brood of Yog-Sothoth"
            , SetAsideCardExists $ CardWithTitle "Brood of Yog-Sothoth"
            ]
          )
        $ ForcedAbility AnyWindow
    ]

instance ActRunner env => RunMessage env TheyMustBeDestroyed where
  runMessage msg a@(TheyMustBeDestroyed attrs) = case msg of
    AdvanceAct aid _ | aid == toId attrs && onSide B attrs ->
      a <$ push (ScenarioResolution $ Resolution 2)
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      a <$ push (AdvanceAct (toId attrs) source)
    _ -> TheyMustBeDestroyed <$> runMessage msg attrs
