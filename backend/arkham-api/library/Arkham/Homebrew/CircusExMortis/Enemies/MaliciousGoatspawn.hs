module Arkham.Homebrew.CircusExMortis.Enemies.MaliciousGoatspawn (maliciousGoatspawn) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (getSealedMoonTokens, hasSealedMoonToken)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype MaliciousGoatspawn = MaliciousGoatspawn EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

maliciousGoatspawn :: EnemyCard MaliciousGoatspawn
maliciousGoatspawn = enemy MaliciousGoatspawn Cards.maliciousGoatspawn

instance HasAbilities MaliciousGoatspawn where
  getAbilities (MaliciousGoatspawn a) =
    -- "for each ☾ token sealed on your investigator card" is 0 with none sealed, so the
    -- gate rides on the window's Who rather than being a separate criteria
    extend1 a
      $ forcedAbility a 1
      $ EnemyAttacks #after (You <> hasSealedMoonToken) AnyEnemyAttack (be a)

instance RunMessage MaliciousGoatspawn where
  runMessage msg e@(MaliciousGoatspawn attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      moons <- length <$> getSealedMoonTokens iid
      repeated moons $ chooseOneM iid $ withI18n do
        countVar 1 $ labeled "takeDamage" $ assignDamage iid (attrs.ability 1) 1
        countVar 1 $ labeled "takeHorror" $ assignHorror iid (attrs.ability 1) 1
      pure e
    _ -> MaliciousGoatspawn <$> liftRunMessage msg attrs
