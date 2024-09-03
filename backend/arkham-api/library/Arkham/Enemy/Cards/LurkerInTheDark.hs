module Arkham.Enemy.Cards.LurkerInTheDark (lurkerInTheDark, LurkerInTheDark (..)) where

import Arkham.Ability
import Arkham.Card.CardType
import Arkham.Constants
import Arkham.DamageEffect
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modified)
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Trait (Trait (Tactic))

newtype LurkerInTheDark = LurkerInTheDark EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lurkerInTheDark :: EnemyCard LurkerInTheDark
lurkerInTheDark =
  enemyWith
    LurkerInTheDark
    Cards.lurkerInTheDark
    (3, Static 2, 1)
    (1, 0)
    (spawnAtL ?~ SpawnAt ConnectedLocation)

instance HasAbilities LurkerInTheDark where
  getAbilities (LurkerInTheDark attrs) = map updateFight (getAbilities attrs)
   where
    updateFight ab =
      if ab.index == AbilityAttack
        then
          ab
            { abilityCriteria =
                abilityCriteria ab
                  <> OnlySources (oneOf [SourceIsAsset #weapon, SourceIsType EventType <> SourceWithTrait Tactic])
            }
        else ab

instance HasModifiersFor LurkerInTheDark where
  getModifiersFor target (LurkerInTheDark a) | isTarget a target = do
    modified
      a
      [ CannotBeDamagedByPlayerSourcesExcept
          $ oneOf [SourceIsAsset #weapon, SourceIsType EventType <> SourceWithTrait Tactic]
      , CannotBeAttackedByPlayerSourcesExcept
          $ oneOf [SourceIsAsset #weapon, SourceIsType EventType <> SourceWithTrait Tactic]
      ]
  getModifiersFor _ _ = pure []

instance RunMessage LurkerInTheDark where
  runMessage msg e@(LurkerInTheDark attrs) = runQueueT $ case msg of
    Msg.EnemyDamage eid damageAssignment | attrs.id == eid -> do
      let amount = damageAssignmentAmount damageAssignment - 1
      if (amount > 0)
        then
          LurkerInTheDark
            <$> liftRunMessage (Msg.EnemyDamage eid $ damageAssignment {damageAssignmentAmount = amount}) attrs
        else pure e
    _ -> LurkerInTheDark <$> liftRunMessage msg attrs
