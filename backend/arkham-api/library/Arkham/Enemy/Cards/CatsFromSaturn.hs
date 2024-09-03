module Arkham.Enemy.Cards.CatsFromSaturn (catsFromSaturn, CatsFromSaturn (..)) where

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner hiding (chooseOne)
import Arkham.Matcher
import Arkham.Message.Lifted
import Arkham.Prelude
import Arkham.Scenarios.DarkSideOfTheMoon.Helpers

newtype CatsFromSaturn = CatsFromSaturn EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

catsFromSaturn :: EnemyCard CatsFromSaturn
catsFromSaturn = enemy CatsFromSaturn Cards.catsFromSaturn (2, Static 2, 2) (1, 0)

instance HasModifiersFor CatsFromSaturn where
  getModifiersFor target (CatsFromSaturn a) | a `is` target = do
    mInvestigator <- selectOne ActiveInvestigator
    x <- maybe getMaxAlarmLevel getAlarmLevel mInvestigator
    pure $ toModifiers a [SwarmingValue x]
  getModifiersFor _ _ = pure []

instance HasAbilities CatsFromSaturn where
  getAbilities (CatsFromSaturn attrs) =
    extend
      attrs
      [ restrictedAbility attrs 1 (exists (be attrs <> IsHost) <> exists (SwarmOf attrs.id))
          $ forced
          $ oneOf
            [ EnemyEvaded #after Anyone (be attrs)
            , EnemyMovedTo #after Anywhere MovedViaAny (be attrs)
            ]
      ]

instance RunMessage CatsFromSaturn where
  runMessage msg e@(CatsFromSaturn attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      swarms <- select $ SwarmOf attrs.id
      lead <- getLead
      chooseOne
        lead
        [targetLabel swarm [Discard Nothing (attrs.ability 1) (toTarget swarm)] | swarm <- swarms]
      pure e
    _ -> CatsFromSaturn <$> liftRunMessage msg attrs
