module Arkham.Enemy.Cards.GuardiansOfTheAbyss.TheNightsUsurper.DreadedShantak (dreadedShantak) where

import Arkham.Enemy.CardDefs.GuardiansOfTheAbyss.TheNightsUsurper qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectWhen)
import Arkham.Matcher

newtype DreadedShantak = DreadedShantak EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

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
