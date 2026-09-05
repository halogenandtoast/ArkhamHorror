{- | The runner behind a debug-authored custom enemy.

Everything it does comes from the def: the printed stats and keywords, plus
whatever abilities are declared under the def's @_abilities@ meta key (see
"Arkham.Custom.Ability"). There is no behaviour here that the def does not
describe.
-}
module Arkham.Custom.Enemy (CustomEnemy (..), customEnemy) where

import Arkham.Card.CardDef (CardDef)
import Arkham.Card.CustomCard (customMeta, customMetaMaybe)
import Arkham.Custom.Ability (
  customAbilities,
  customModifiers,
  isCustomAbility,
  runCustomAbility,
  runCustomHandlers,
 )
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher (InvestigatorMatcher (Anyone), PreyMatcher (Prey))

newtype CustomEnemy = CustomEnemy EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | Prey and spawn instructions are attrs fields rather than keywords, so they
come from the def's meta the way an asset's health does.
-}
customEnemy :: CardDef -> EnemyCard CustomEnemy
customEnemy def =
  enemyWith CustomEnemy def \a ->
    a
      { enemyPrey = customMeta "prey" (Prey Anyone) def
      , enemySpawnAt = customMetaMaybe @SpawnAt "spawnAt" def
      }

instance HasModifiersFor CustomEnemy where
  getModifiersFor (CustomEnemy a) = customModifiers a

instance HasAbilities CustomEnemy where
  getAbilities (CustomEnemy a) = extend a (customAbilities a)

instance RunMessage CustomEnemy where
  runMessage msg x@(CustomEnemy attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) idx | isCustomAbility attrs idx -> do
      runCustomAbility attrs iid idx
      pure x
    _ -> do
      runCustomHandlers attrs msg
      CustomEnemy <$> liftRunMessage msg attrs
