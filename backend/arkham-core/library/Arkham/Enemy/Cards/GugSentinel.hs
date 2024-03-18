module Arkham.Enemy.Cards.GugSentinel (gugSentinel, GugSentinel (..)) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Game.Helpers (perPlayer)
import Arkham.Helpers.Modifiers
import Arkham.Matcher

newtype GugSentinel = GugSentinel EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gugSentinel :: EnemyCard GugSentinel
gugSentinel = enemy GugSentinel Cards.gugSentinel (5, Static 2, 3) (2, 1)

instance HasModifiersFor GugSentinel where
  getModifiersFor target (GugSentinel attrs) | attrs `is` target = do
    health <- perPlayer 2
    pure $ toModifiers attrs [HealthModifier health]
  getModifiersFor _ _ = pure []

instance HasAbilities GugSentinel where
  getAbilities (GugSentinel attrs) =
    extend
      attrs
      [ restrictedAbility attrs 1 (exists $ InvestigatorAt (locationWithEnemy attrs.id))
          $ forced
          $ EnemyReadies #after (be attrs)
      ]

instance RunMessage GugSentinel where
  runMessage msg e@(GugSentinel attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectEach (InvestigatorAt $ locationWithEnemy $ toId attrs) \iid -> do
        assignHorror iid (attrs.ability 1) 1

      pure e
    _ -> GugSentinel <$> lift (runMessage msg attrs)
