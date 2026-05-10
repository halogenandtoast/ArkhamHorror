module Arkham.Event.Events.BloodRite (bloodRite) where

import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Capability
import Arkham.Classes.HasGame
import Arkham.Cost hiding (discardedCards)
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Cost (getSpendableResources)
import Arkham.Helpers.Enemy (getDamageableEnemies)
import Arkham.Helpers.Modifiers
import Arkham.I18n
import Arkham.Investigator.Projection ()
import Arkham.Matcher hiding (NonAttackDamageEffect)

newtype BloodRite = BloodRite EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bloodRite :: EventCard BloodRite
bloodRite = event BloodRite Cards.bloodRite

bloodRiteLimit :: HasGame m => EventAttrs -> m Int
bloodRiteLimit attrs = do
  use3 <- getMetaMaybe False attrs.cardId "use3"
  pure $ if use3 then 3 else 2

instance RunMessage BloodRite where
  runMessage msg e@(BloodRite attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      drawCards iid attrs =<< bloodRiteLimit attrs
      push $ PayForCardAbility iid (toSource attrs) attrs.windows 1 (DiscardCardPayment [])
      pure e
    PayForCardAbility iid (isSource attrs -> True) windows 1 (DiscardCardPayment discards) -> do
      limit <- bloodRiteLimit attrs
      if length discards == limit
        then doStep (length discards) msg
        else chooseOneM iid do
          targetsM iid.discardable \card -> do
            discardCard iid attrs card
            push $ PayForCardAbility iid (toSource attrs) windows 1 (DiscardCardPayment $ card : discards)
          withI18n (countVar (length discards) $ labeled' "continueHavingDiscarded" $ doStep (length discards) msg)
      pure e
    DoStep n msg'@(PayForCardAbility iid (isSource attrs -> True) _ 1 _) | n > 0 -> do
      resources <- getSpendableResources iid
      enemies <- getDamageableEnemies iid attrs (enemyAtLocationWith iid)
      concealed <- getConcealed (ForExpose $ toSource iid) iid
      chooseOneM iid $ cardI18n $ scope "bloodRite" do
        whenM (can.gain.resources FromPlayerCardEffect iid) do
          labeled' "gainResource" $ gainResources iid attrs 1 >> doStep (n - 1) msg'
        when ((notNull enemies || notNull concealed) && resources > 0) do
          labeled' "spendDealDamage" do
            spendResources iid 1
            chooseDamageEnemy iid attrs (locationWithInvestigator iid) AnyEnemy 1
            doStep (n - 1) msg'
      pure e
    _ -> BloodRite <$> liftRunMessage msg attrs
