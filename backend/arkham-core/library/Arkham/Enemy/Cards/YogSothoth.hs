module Arkham.Enemy.Cards.YogSothoth (yogSothoth, yogSothothEffect, YogSothoth (..)) where

import Arkham.Ability
import Arkham.Classes
import Arkham.Effect.Runner hiding (EnemyAttacks)
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Prelude

newtype YogSothoth = YogSothoth EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

yogSothoth :: EnemyCard YogSothoth
yogSothoth = enemyWith YogSothoth Cards.yogSothoth (4, Static 4, 0) (1, 5) (evadeL .~ Nothing)

instance HasModifiersFor YogSothoth where
  getModifiersFor target (YogSothoth a) | isTarget a target = do
    healthModifier <- getPlayerCountValue (PerPlayer 6)
    pure
      $ toModifiers a [HealthModifier healthModifier, CannotMakeAttacksOfOpportunity, CannotBeEvaded]
  getModifiersFor _ _ = pure []

instance HasAbilities YogSothoth where
  getAbilities (YogSothoth attrs) =
    withBaseAbilities
      attrs
      [mkAbility attrs 1 $ freeReaction (EnemyAttacks #when You AnyEnemyAttack $ be attrs)]

instance RunMessage YogSothoth where
  runMessage msg e@(YogSothoth attrs@EnemyAttrs {..}) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ Label
            ( "Discard the top "
                <> tshow discardCount
                <> " cards and take "
                <> tshow (enemySanityDamage - discardCount)
                <> " horror"
            )
            [ createCardEffect Cards.yogSothoth (Just $ EffectInt discardCount) source iid
            , DiscardTopOfDeck iid discardCount (toAbilitySource attrs 1) Nothing
            ]
          | discardCount <- [0 .. enemySanityDamage]
          ]
      pure e
    _ -> YogSothoth <$> runMessage msg attrs

newtype YogSothothEffect = YogSothothEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

yogSothothEffect :: EffectArgs -> YogSothothEffect
yogSothothEffect = cardEffect YogSothothEffect Cards.yogSothoth

instance HasModifiersFor YogSothothEffect where
  getModifiersFor target (YogSothothEffect attrs) = case effectMetadata attrs of
    Just (EffectInt n) -> case target of
      EnemyTarget eid -> case effectSource attrs of
        EnemySource eid' | eid' == eid -> do
          pure $ toModifiers attrs [HorrorDealt (-n)]
        _ -> pure []
      _ -> pure []
    _ -> pure []

instance RunMessage YogSothothEffect where
  runMessage msg e@(YogSothothEffect attrs) = case msg of
    Msg.DeckHasNoCards iid _ | isTarget attrs (InvestigatorTarget iid) -> do
      push (DrivenInsane iid)
      pure e
    _ -> YogSothothEffect <$> runMessage msg attrs
