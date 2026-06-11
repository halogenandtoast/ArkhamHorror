module Arkham.Act.Cards.TheEscapeTheLabyrinthsOfLunacy (theEscapeTheLabyrinthsOfLunacy) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher

newtype TheEscapeTheLabyrinthsOfLunacy = TheEscapeTheLabyrinthsOfLunacy ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theEscapeTheLabyrinthsOfLunacy :: ActCard TheEscapeTheLabyrinthsOfLunacy
theEscapeTheLabyrinthsOfLunacy =
  act (3, A) TheEscapeTheLabyrinthsOfLunacy Cards.theEscapeTheLabyrinthsOfLunacy Nothing

-- Objective - If Eixodolon has no remaining health, advance.
instance HasAbilities TheEscapeTheLabyrinthsOfLunacy where
  getAbilities = actAbilities \a ->
    [ restricted
        a
        1
        (exists $ enemyIs Enemies.eixodolon <> EnemyWithRemainingHealth (EqualTo $ Static 0))
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage TheEscapeTheLabyrinthsOfLunacy where
  runMessage msg a@(TheEscapeTheLabyrinthsOfLunacy attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advanceVia #other attrs (attrs.ability 1)
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R2
      pure a
    _ -> TheEscapeTheLabyrinthsOfLunacy <$> liftRunMessage msg attrs
