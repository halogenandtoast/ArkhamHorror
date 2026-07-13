module Arkham.Enemy.Cards.TheBOOGEYMANDarkMatter (theBOOGEYMANDarkMatter) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Keyword qualified as Keyword

newtype TheBOOGEYMANDarkMatter = TheBOOGEYMANDarkMatter EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theBOOGEYMANDarkMatter :: EnemyCard TheBOOGEYMANDarkMatter
theBOOGEYMANDarkMatter = enemy TheBOOGEYMANDarkMatter Cards.theBOOGEYMANDarkMatter

instance HasModifiersFor TheBOOGEYMANDarkMatter where
  getModifiersFor (TheBOOGEYMANDarkMatter a) =
    modifySelf
      a
      [ AddKeyword Keyword.Massive
      , AddKeyword Keyword.Hunter
      , CannotBeAttacked
      , CannotBeDamaged
      , CannotBeEvaded
      ]

instance RunMessage TheBOOGEYMANDarkMatter where
  runMessage msg (TheBOOGEYMANDarkMatter attrs) =
    runQueueT $ TheBOOGEYMANDarkMatter <$> liftRunMessage msg attrs
