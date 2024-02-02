module Arkham.Act.Cards.TheyMustBeDestroyed (
  TheyMustBeDestroyed (..),
  theyMustBeDestroyed,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Matcher
import Arkham.Resolution

newtype TheyMustBeDestroyed = TheyMustBeDestroyed ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

theyMustBeDestroyed :: ActCard TheyMustBeDestroyed
theyMustBeDestroyed =
  act (2, A) TheyMustBeDestroyed Cards.theyMustBeDestroyed Nothing

instance HasAbilities TheyMustBeDestroyed where
  getAbilities (TheyMustBeDestroyed x)
    | onSide A x =
        [ restrictedAbility
            x
            1
            ( Negate
                $ AnyCriterion
                  [ EnemyCriteria
                      $ EnemyExists
                      $ EnemyWithTitle
                        "Brood of Yog-Sothoth"
                  , SetAsideCardExists $ CardWithTitle "Brood of Yog-Sothoth"
                  ]
            )
            $ ForcedAbility AnyWindow
        ]
  getAbilities _ = []

instance RunMessage TheyMustBeDestroyed where
  runMessage msg a@(TheyMustBeDestroyed attrs) = case msg of
    AdvanceAct aid _ _
      | aid == toId attrs && onSide B attrs ->
          a <$ push (ScenarioResolution $ Resolution 2)
    UseCardAbility _ source 1 _ _
      | isSource attrs source ->
          a <$ push (AdvanceAct (toId attrs) source AdvancedWithOther)
    _ -> TheyMustBeDestroyed <$> runMessage msg attrs
