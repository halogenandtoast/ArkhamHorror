module Arkham.Types.Enemy.Cards.YogSothoth
  ( yogSothoth
  , YogSothoth(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Game.Helpers
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.Source

newtype YogSothoth = YogSothoth EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

yogSothoth :: EnemyCard YogSothoth
yogSothoth = enemy YogSothoth Cards.yogSothoth (4, Static 4, 0) (1, 5)

instance HasCount PlayerCount env () => HasModifiersFor env YogSothoth where
  getModifiersFor _ target (YogSothoth a) | isTarget a target = do
    healthModifier <- getPlayerCountValue (PerPlayer 6)
    pure $ toModifiers
      a
      [ HealthModifier healthModifier
      , CannotMakeAttacksOfOpportunity
      , CannotBeEvaded
      ]
  getModifiersFor _ _ _ = pure []

instance EnemyAttrsHasActions env => HasActions env YogSothoth where
  getActions i window (YogSothoth attrs) = getActions i window attrs

instance EnemyAttrsRunMessage env => RunMessage env YogSothoth where
  runMessage msg e@(YogSothoth attrs@EnemyAttrs {..}) = case msg of
    PerformEnemyAttack iid eid _ | eid == enemyId -> e <$ pushAll
      (chooseOne
          iid
          [ Label
              ("Discard the top "
              <> tshow discardCount
              <> " cards and take "
              <> tshow (enemySanityDamage - discardCount)
              <> " horror"
              )
              [ DiscardTopOfDeck iid discardCount (Just $ toTarget attrs)
              , InvestigatorAssignDamage
                iid
                (EnemySource enemyId)
                DamageAny
                enemyHealthDamage
                (enemySanityDamage - discardCount)
              ]
          | discardCount <- [0 .. enemySanityDamage]
          ]
      : [After (EnemyAttack iid eid DamageAny)]
      )
    DeckHasNoCards iid (Just target) | isTarget attrs target ->
      e <$ push (DrivenInsane iid)
    _ -> YogSothoth <$> runMessage msg attrs
