module Arkham.Enemy.Cards.SpiderOfLeng (
  spiderOfLeng,
  SpiderOfLeng (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher

newtype SpiderOfLeng = SpiderOfLeng EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

spiderOfLeng :: EnemyCard SpiderOfLeng
spiderOfLeng = enemy SpiderOfLeng Cards.spiderOfLeng (3, Static 4, 3) (1, 1)

instance HasAbilities SpiderOfLeng where
  getAbilities (SpiderOfLeng x) =
    extend x [mkAbility x 1 $ forced $ PhaseEnds #when #enemy]

instance RunMessage SpiderOfLeng where
  runMessage msg e@(SpiderOfLeng attrs) = case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      lead <- getLead
      player <- getLeadPlayer
      swarmsOfSpiders <- selectList $ enemyIs Cards.swarmOfSpiders
      if null swarmsOfSpiders
        then push $ findEncounterCard lead attrs [FromEncounterDeck, FromEncounterDiscard] Cards.swarmOfSpiders
        else do
          push
            $ chooseOneAtATime player [targetLabel eid [PlaceSwarmCards lead eid 1] | eid <- swarmsOfSpiders]
      pure e
    FoundEncounterCard _iid (isTarget attrs -> True) card -> do
      creation <- createEnemy card (locationWithEnemy $ toId attrs)
      pushAll
        [ abilityModifier (attrs.ability 1) creation.enemy NoInitialSwarm
        , toMessage creation
        ]
      pure e
    _ -> SpiderOfLeng <$> runMessage msg attrs
