module Arkham.Homebrew.AgesUnwound.Enemies.SheldonsFinest (sheldonsFinest) where

import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Cards
import Arkham.Matcher

newtype SheldonsFinest = SheldonsFinest EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

-- | Hunter and Retaliate are on the card def.
sheldonsFinest :: EnemyCard SheldonsFinest
sheldonsFinest = enemy SheldonsFinest Cards.sheldonsFinest

{- | "While Sheldon's Finest are engaged with you, you get -1 [agility] and cannot
move or be moved."

Both halves of "cannot move or be moved": 'CannotMove' bars the investigator's
own movement, 'CannotBeMoved' bars anything that would move them.
-}
instance HasModifiersFor SheldonsFinest where
  getModifiersFor (SheldonsFinest a) =
    modifySelect
      a
      (investigatorEngagedWith a.id)
      [SkillModifier #agility (-1), CannotMove, CannotBeMoved]

instance RunMessage SheldonsFinest where
  runMessage msg (SheldonsFinest attrs) = SheldonsFinest <$> runMessage msg attrs
