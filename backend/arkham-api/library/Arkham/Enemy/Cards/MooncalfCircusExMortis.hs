module Arkham.Enemy.Cards.MooncalfCircusExMortis (mooncalfCircusExMortis) where

import Arkham.Campaigns.CircusExMortis.Helpers
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher

newtype MooncalfCircusExMortis = MooncalfCircusExMortis EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

mooncalfCircusExMortis :: EnemyCard MooncalfCircusExMortis
mooncalfCircusExMortis =
  enemyWith MooncalfCircusExMortis Cards.mooncalfCircusExMortis
    $ preyL
    .~ Prey (InvestigatorWithSealedChaosToken #moon)

instance HasModifiersFor MooncalfCircusExMortis where
  getModifiersFor (MooncalfCircusExMortis a) = do
    investigators <- select $ InvestigatorAt (locationWithEnemy a)
    n <- sum <$> traverse (fmap length . getSealedMoonTokens) investigators
    modifySelf a $ AddKeyword Keyword.Hunter : [EnemyFight n | n > 0] <> [EnemyEvade n | n > 0]

instance RunMessage MooncalfCircusExMortis where
  runMessage msg (MooncalfCircusExMortis attrs) = MooncalfCircusExMortis <$> runMessage msg attrs
