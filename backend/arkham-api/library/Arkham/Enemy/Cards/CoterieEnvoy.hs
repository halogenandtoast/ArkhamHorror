module Arkham.Enemy.Cards.CoterieEnvoy (coterieEnvoy) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyDefeated)
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Matcher

newtype CoterieEnvoy = CoterieEnvoy EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

coterieEnvoy :: EnemyCard CoterieEnvoy
coterieEnvoy = enemy CoterieEnvoy Cards.coterieEnvoy (2, Static 3, 2) (1, 0)

instance HasModifiersFor CoterieEnvoy where
  getModifiersFor (CoterieEnvoy a) = do
    when a.ready do
      withLocationOf a \lid -> do
        modified_ a lid [CampaignModifier "noExposeAt"]

instance HasAbilities CoterieEnvoy where
  getAbilities (CoterieEnvoy a) =
    extend1 a
      $ restricted a 1 (exists ConcealedCardAny)
      $ freeReaction
      $ EnemyDefeated #after You ByAny (be a)

instance RunMessage CoterieEnvoy where
  runMessage msg e@(CoterieEnvoy attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseRevealConcealedAt iid (attrs.ability 1) Anywhere
      pure e
    _ -> CoterieEnvoy <$> liftRunMessage msg attrs
