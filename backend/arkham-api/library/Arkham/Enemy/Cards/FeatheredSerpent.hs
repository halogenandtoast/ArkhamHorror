module Arkham.Enemy.Cards.FeatheredSerpent (featheredSerpent) where

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Enemy.Types (Field (EnemyHealthDamage))
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen)
import Arkham.Keyword qualified as Keyword
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Scenarios.HeartOfTheElders.Helpers
import Arkham.Token

newtype FeatheredSerpent = FeatheredSerpent EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

featheredSerpent :: EnemyCard FeatheredSerpent
featheredSerpent = enemy FeatheredSerpent Cards.featheredSerpent (0, Static 3, 3) (1, 0)

instance HasModifiersFor FeatheredSerpent where
  getModifiersFor (FeatheredSerpent a) = do
    selectOne (locationIs Locations.mouthOfKnYanTheCavernsMaw) >>= traverse_ \loc -> do
      pillars <- fieldMap LocationTokens (countTokens Pillar) loc
      modifySelfWhen a (pillars > 0) [EnemyFight pillars]
      modifySelfWhen a (pillars >= 3) [AddKeyword Keyword.Alert, AddKeyword Keyword.Hunter]

instance HasAbilities FeatheredSerpent where
  getAbilities (FeatheredSerpent a) =
    extend1 a
      $ scenarioI18n
      $ withI18nTooltip "featheredSerpent.evade"
      $ restricted a 1 EvadeCriteria evadeAction_

instance RunMessage FeatheredSerpent where
  runMessage msg e@(FeatheredSerpent attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      hasMysteriousScepter <- getHasSupply iid MysteriousScepter
      if hasMysteriousScepter
        then do
          automaticallyEvadeEnemy iid attrs
          dmg <- field EnemyHealthDamage attrs.id
          enemies <- select $ enemy_ $ at_ (locationWithEnemy attrs) <> not_ (be attrs.id)
          chooseTargetM iid enemies $ nonAttackEnemyDamage (Just iid) (toSource attrs) dmg
          pure e
        else FeatheredSerpent <$> liftRunMessage msg attrs
    _ -> FeatheredSerpent <$> liftRunMessage msg attrs
