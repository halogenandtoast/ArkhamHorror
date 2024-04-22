module Arkham.Act.Cards.TheyMustBeDestroyed (TheyMustBeDestroyed (..), theyMustBeDestroyed) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Matcher
import Arkham.Prelude

newtype TheyMustBeDestroyed = TheyMustBeDestroyed ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theyMustBeDestroyed :: ActCard TheyMustBeDestroyed
theyMustBeDestroyed = act (2, A) TheyMustBeDestroyed Cards.theyMustBeDestroyed Nothing

instance HasAbilities TheyMustBeDestroyed where
  getAbilities (TheyMustBeDestroyed x) | onSide A x = do
    [ restrictedAbility
        x
        1
        ( not_
            $ oneOf
              [ exists $ EnemyWithTitle "Brood of Yog-Sothoth"
              , SetAsideCardExists $ CardWithTitle "Brood of Yog-Sothoth"
              ]
        )
        $ Objective
        $ forced AnyWindow
      ]
  getAbilities _ = []

instance RunMessage TheyMustBeDestroyed where
  runMessage msg a@(TheyMustBeDestroyed attrs) = case msg of
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      a <$ push R2
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      a <$ push (AdvanceAct (toId attrs) source AdvancedWithOther)
    _ -> TheyMustBeDestroyed <$> runMessage msg attrs
