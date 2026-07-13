module Arkham.Homebrew.CircusExMortis.Enemies.NewMoonCarny (newMoonCarny) where

import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Keyword qualified as Keyword

newtype NewMoonCarny = NewMoonCarny EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

newMoonCarny :: EnemyCard NewMoonCarny
newMoonCarny = enemy NewMoonCarny Cards.newMoonCarny

instance HasModifiersFor NewMoonCarny where
  getModifiersFor (NewMoonCarny a) =
    modifySelf a
      $ AddKeyword Keyword.Hunter
      : case a.damage of
        1 -> [AddKeyword Keyword.Retaliate, AddKeyword Keyword.Alert]
        2 -> [EnemyFight 1, EnemyEvade 1]
        _ -> []

instance RunMessage NewMoonCarny where
  runMessage msg (NewMoonCarny attrs) =
    NewMoonCarny <$> runMessage msg attrs
