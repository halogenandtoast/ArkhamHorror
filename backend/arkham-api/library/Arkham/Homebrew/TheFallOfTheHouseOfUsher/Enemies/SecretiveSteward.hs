module Arkham.Homebrew.TheFallOfTheHouseOfUsher.Enemies.SecretiveSteward (secretiveSteward) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.CardDefs.Enemies qualified as Cards
import Arkham.Matcher
import Arkham.Token (Token (Clue))

newtype SecretiveSteward = SecretiveSteward EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

secretiveSteward :: EnemyCard SecretiveSteward
secretiveSteward = enemy SecretiveSteward Cards.secretiveSteward & setPrey MostClues

instance HasAbilities SecretiveSteward where
  getAbilities (SecretiveSteward a) =
    extend1 a $ restricted a 1 (thisExists a ReadyEnemy) $ forced $ PhaseBegins #when #enemy

instance RunMessage SecretiveSteward where
  runMessage msg e@(SecretiveSteward attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      mlid <- selectOne $ locationWithEnemy attrs.id
      for_ mlid \lid -> do
        iids <- select $ investigatorAt lid <> InvestigatorWithClues (atLeast 1)
        for_ iids \iid -> moveTokens (attrs.ability 1) iid lid Clue 1
      pure e
    _ -> SecretiveSteward <$> liftRunMessage msg attrs
