module Arkham.Enemy.Cards.TommyMalloy (
  tommyMalloy,
  TommyMalloy (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card.CardCode
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype TommyMalloy = TommyMalloy EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tommyMalloy :: EnemyCard TommyMalloy
tommyMalloy =
  enemyWith
    TommyMalloy
    Cards.tommyMalloy
    (2, Static 3, 3)
    (2, 0)
    (\a -> a & preyL .~ BearerOf (toId a))

instance HasAbilities TommyMalloy where
  getAbilities (TommyMalloy attrs) =
    withBaseAbilities
      attrs
      [ mkAbility attrs 1
          $ ForcedAbility
          $ EnemyTakeDamage
            Timing.When
            AnyDamageEffect
            (EnemyWithId $ toId attrs)
            AnySource
      ]

instance RunMessage TommyMalloy where
  runMessage msg e@(TommyMalloy attrs) = case msg of
    UseCardAbility _ source 1 _ _
      | isSource attrs source ->
          e
            <$ push
              ( CreateEffect
                  (toCardCode attrs)
                  Nothing
                  (toSource attrs)
                  (toTarget attrs)
              )
    _ -> TommyMalloy <$> runMessage msg attrs
