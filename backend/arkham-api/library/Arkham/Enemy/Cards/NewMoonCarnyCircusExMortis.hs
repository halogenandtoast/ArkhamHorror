module Arkham.Enemy.Cards.NewMoonCarnyCircusExMortis (newMoonCarnyCircusExMortis) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Keyword qualified as Keyword

newtype NewMoonCarnyCircusExMortis = NewMoonCarnyCircusExMortis EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

newMoonCarnyCircusExMortis :: EnemyCard NewMoonCarnyCircusExMortis
newMoonCarnyCircusExMortis = enemy NewMoonCarnyCircusExMortis Cards.newMoonCarnyCircusExMortis

instance HasModifiersFor NewMoonCarnyCircusExMortis where
  getModifiersFor (NewMoonCarnyCircusExMortis a) =
    modifySelf a
      $ AddKeyword Keyword.Hunter
      : case a.damage of
        1 -> [AddKeyword Keyword.Retaliate, AddKeyword Keyword.Alert]
        2 -> [EnemyFight 1, EnemyEvade 1]
        _ -> []

instance RunMessage NewMoonCarnyCircusExMortis where
  runMessage msg (NewMoonCarnyCircusExMortis attrs) =
    NewMoonCarnyCircusExMortis <$> runMessage msg attrs
