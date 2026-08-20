module Arkham.Act.Cards.TheLabyrinthsOfLunacy.TheEscape (theEscape) where

import Arkham.Ability
import Arkham.Act.CardDefs.TheLabyrinthsOfLunacy qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher

newtype TheEscape = TheEscape ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theEscape :: ActCard TheEscape
theEscape =
  act (3, A) TheEscape Cards.theEscape Nothing

-- Objective - If Eixodolon has no remaining health, advance.
instance HasAbilities TheEscape where
  getAbilities = actAbilities \a ->
    [ restricted
        a
        1
        (exists $ enemyIs Enemies.eixodolon <> EnemyWithRemainingHealth (EqualTo $ Static 0))
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage TheEscape where
  runMessage msg a@(TheEscape attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advanceVia #other attrs (attrs.ability 1)
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R2
      pure a
    _ -> TheEscape <$> liftRunMessage msg attrs
