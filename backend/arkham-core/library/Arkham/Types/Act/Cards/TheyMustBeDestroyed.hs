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
import Arkham.Types.Message
import Arkham.Types.Resolution
import Arkham.Types.Restriction

newtype TheyMustBeDestroyed = TheyMustBeDestroyed ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor env)

theyMustBeDestroyed :: ActCard TheyMustBeDestroyed
theyMustBeDestroyed =
  act (2, A) TheyMustBeDestroyed Cards.theyMustBeDestroyed Nothing

instance HasActions TheyMustBeDestroyed where
  getActions (TheyMustBeDestroyed x) =
    restrictedAbility
        x
        1
        (AnyRestriction
          [ EnemyExists $ EnemyWithTitle "Brood of Yog-Sothoth"
          , SetAsideCardExists $ CardWithTitle "Brood of Yog-Sothoth"
          ]
        )
        (Objective $ ForcedAbility AnyWindow)
      : getActions x

instance ActRunner env => RunMessage env TheyMustBeDestroyed where
  runMessage msg a@(TheyMustBeDestroyed attrs@ActAttrs {..}) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      a <$ push (AdvanceAct (toId attrs) (toSource attrs))
    AdvanceAct aid _ | aid == actId && onSide B attrs ->
      a <$ push (ScenarioResolution $ Resolution 2)
    _ -> TheyMustBeDestroyed <$> runMessage msg attrs
