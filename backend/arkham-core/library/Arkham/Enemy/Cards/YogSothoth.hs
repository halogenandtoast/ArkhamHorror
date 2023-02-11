module Arkham.Enemy.Cards.YogSothoth
  ( yogSothoth
  , YogSothoth(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Card
import Arkham.Classes
import Arkham.Cost
import Arkham.EffectMetadata
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message hiding (EnemyAttacks)
import Arkham.Target
import Arkham.Timing qualified as Timing

newtype YogSothoth = YogSothoth EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

yogSothoth :: EnemyCard YogSothoth
yogSothoth = enemy YogSothoth Cards.yogSothoth (4, Static 4, 0) (1, 5)

instance HasModifiersFor YogSothoth where
  getModifiersFor target (YogSothoth a) | isTarget a target = do
    healthModifier <- getPlayerCountValue (PerPlayer 6)
    pure $ toModifiers
      a
      [ HealthModifier healthModifier
      , CannotMakeAttacksOfOpportunity
      , CannotBeEvaded
      ]
  getModifiersFor _ _ = pure []

instance HasAbilities YogSothoth where
  getAbilities (YogSothoth attrs) = withBaseAbilities
    attrs
    [ mkAbility attrs 1
        $ ReactionAbility
            (EnemyAttacks Timing.When You AnyEnemyAttack $ EnemyWithId $ toId attrs)
            Free
    ]

instance RunMessage YogSothoth where
  runMessage msg e@(YogSothoth attrs@EnemyAttrs {..}) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      e <$ push
        (chooseOne
          iid
          [ Label
              ("Discard the top "
              <> tshow discardCount
              <> " cards and take "
              <> tshow (enemySanityDamage - discardCount)
              <> " horror"
              )
              [ CreateEffect
                (toCardCode attrs)
                (Just $ EffectInt discardCount)
                source
                (InvestigatorTarget iid)
              , DiscardTopOfDeck iid discardCount (toAbilitySource attrs 1) Nothing
              ]
          | discardCount <- [0 .. enemySanityDamage]
          ]
        )
    _ -> YogSothoth <$> runMessage msg attrs
