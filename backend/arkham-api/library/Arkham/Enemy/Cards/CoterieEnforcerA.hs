module Arkham.Enemy.Cards.CoterieEnforcerA (coterieEnforcerA) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Card.CardCode
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype CoterieEnforcerA = CoterieEnforcerA EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

coterieEnforcerA :: EnemyCard CoterieEnforcerA
coterieEnforcerA = enemy CoterieEnforcerA Cards.coterieEnforcerA (4, Static 3, 1) (0, 1)

instance HasAbilities CoterieEnforcerA where
  getAbilities (CoterieEnforcerA a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ mapOneOf
        (\c -> CampaignEvent #after (Just You) ("exposed[" <> unCardCode (toCardCode c) <> "]"))
        (filter (`notElem` [Cards.coterieEnforcerA, Cards.coterieEnforcerB]) allConcealedCardDefs)

instance RunMessage CoterieEnforcerA where
  runMessage msg e@(CoterieEnforcerA attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      initiateEnemyAttack attrs (attrs.ability 1) iid
      pure e
    _ -> CoterieEnforcerA <$> liftRunMessage msg attrs
