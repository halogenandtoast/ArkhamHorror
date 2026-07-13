module Arkham.Homebrew.CircusExMortis.Enemies.Mooncalf (mooncalf) where

import Arkham.Homebrew.CircusExMortis.Helpers
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher

newtype Mooncalf = Mooncalf EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

mooncalf :: EnemyCard Mooncalf
mooncalf =
  enemyWith Mooncalf Cards.mooncalf
    $ preyL
    .~ Prey (InvestigatorWithSealedChaosToken #moon)

instance HasModifiersFor Mooncalf where
  getModifiersFor (Mooncalf a) = do
    investigators <- select $ InvestigatorAt (locationWithEnemy a)
    n <- sum <$> traverse (fmap length . getSealedMoonTokens) investigators
    modifySelf a $ AddKeyword Keyword.Hunter : [EnemyFight n | n > 0] <> [EnemyEvade n | n > 0]

instance RunMessage Mooncalf where
  runMessage msg (Mooncalf attrs) = Mooncalf <$> runMessage msg attrs
