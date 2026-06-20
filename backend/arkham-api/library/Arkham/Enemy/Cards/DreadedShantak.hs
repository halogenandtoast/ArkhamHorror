module Arkham.Enemy.Cards.DreadedShantak (dreadedShantak) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectWhen)
import Arkham.Matcher

newtype DreadedShantak = DreadedShantak EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dreadedShantak :: EnemyCard DreadedShantak
dreadedShantak = enemy DreadedShantak Cards.dreadedShantak

instance HasModifiersFor DreadedShantak where
  getModifiersFor (DreadedShantak a) = do
    modifySelectWhen
      a
      a.ready
      Anyone
      [ CannotTriggerAbilityMatching
          $ AbilityOnLocation (locationWithEnemy a)
          <> AbilityOneOf [AbilityIsActionAbility, AbilityIsReactionAbility]
      ]

instance RunMessage DreadedShantak where
  runMessage msg (DreadedShantak attrs) = DreadedShantak <$> runMessage msg attrs
