module Arkham.Enemy.Cards.CoterieEnforcerB (coterieEnforcerB) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Card.CardCode
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype CoterieEnforcerB = CoterieEnforcerB EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

coterieEnforcerB :: EnemyCard CoterieEnforcerB
coterieEnforcerB = enemy CoterieEnforcerB Cards.coterieEnforcerB (4, Static 3, 1) (0, 1)

instance HasAbilities CoterieEnforcerB where
  getAbilities (CoterieEnforcerB a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ mapOneOf
        (\c -> CampaignEvent #after (Just You) ("exposed[" <> unCardCode (toCardCode c) <> "]"))
        (filter (`notElem` [Cards.coterieEnforcerA, Cards.coterieEnforcerB]) allConcealedCardDefs)

instance RunMessage CoterieEnforcerB where
  runMessage msg e@(CoterieEnforcerB attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      initiateEnemyAttack attrs (attrs.ability 1) iid
      pure e
    _ -> CoterieEnforcerB <$> liftRunMessage msg attrs
