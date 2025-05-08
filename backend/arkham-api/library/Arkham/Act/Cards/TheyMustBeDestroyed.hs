module Arkham.Act.Cards.TheyMustBeDestroyed (theyMustBeDestroyed) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Matcher

newtype TheyMustBeDestroyed = TheyMustBeDestroyed ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theyMustBeDestroyed :: ActCard TheyMustBeDestroyed
theyMustBeDestroyed = act (2, A) TheyMustBeDestroyed Cards.theyMustBeDestroyed Nothing

instance HasAbilities TheyMustBeDestroyed where
  getAbilities (TheyMustBeDestroyed x) | onSide A x = do
    [ restricted
        x
        1
        ( not_
            $ oneOf
              [ exists $ InPlayEnemy $ EnemyWithTitle "Brood of Yog-Sothoth"
              , SetAsideCardExists $ CardWithTitle "Brood of Yog-Sothoth"
              ]
        )
        $ Objective
        $ forced AnyWindow
      ]
  getAbilities _ = []

instance RunMessage TheyMustBeDestroyed where
  runMessage msg a@(TheyMustBeDestroyed attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R2
      pure a
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    _ -> TheyMustBeDestroyed <$> liftRunMessage msg attrs
