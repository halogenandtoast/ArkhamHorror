module Arkham.Types.Enemy.Cards.YogSothoth
  ( yogSothoth
  , YogSothoth(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.EffectMetadata
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Game.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message hiding (EnemyAttacks)
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing

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

instance HasAbilities env YogSothoth where
  getAbilities iid window (YogSothoth attrs) =
    withBaseAbilities iid window attrs $ pure
      [ mkAbility attrs 1
          $ ReactionAbility
              (EnemyAttacks Timing.When You $ EnemyWithId $ toId attrs)
              Free
      ]

instance EnemyAttrsRunMessage env => RunMessage env YogSothoth where
  runMessage msg e@(YogSothoth attrs@EnemyAttrs {..}) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
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
              , DiscardTopOfDeck iid discardCount Nothing
              ]
          | discardCount <- [0 .. enemySanityDamage]
          ]
        )
    _ -> YogSothoth <$> runMessage msg attrs
