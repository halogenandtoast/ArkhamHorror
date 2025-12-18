module Arkham.Campaigns.TheScarletKeys.Key.Cards.TheRuinousChime (theRuinousChime) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Key.Cards qualified as Cards
import Arkham.Campaigns.TheScarletKeys.Key.Import.Lifted
import Arkham.Matcher hiding (key)
import Arkham.Message.Lifted.Choose

newtype TheRuinousChime = TheRuinousChime ScarletKeyAttrs
  deriving anyclass (IsScarletKey, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theRuinousChime :: ScarletKeyCard TheRuinousChime
theRuinousChime = key TheRuinousChime Cards.theRuinousChime

instance HasAbilities TheRuinousChime where
  getAbilities (TheRuinousChime a) = case a.bearer of
    InvestigatorTarget iid | not a.shifted ->
      case a.stability of
        Stable ->
          [ restricted
              a
              1
              (exists $ EnemyAt (locationWithInvestigator iid) <> EnemyCanBeEvadedBy (a.ability 1))
              $ FastAbility Free
          ]
        Unstable -> [restricted a 1 (youExist (InvestigatorWithId iid)) $ FastAbility Free]
    _ -> []

instance RunMessage TheRuinousChime where
  runMessage msg k@(TheRuinousChime attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> liftRunMessage (CampaignSpecific "shift[09770]" Null) k
    CampaignSpecific "shift[09770]" _ -> do
      shiftKey attrs do
        when attrs.unstable do
          eachInvestigator \iid -> directHorror iid (attrs.ability 1) 1
          withInvestigatorBearer attrs (`flipOver` attrs)
        when attrs.stable do
          withInvestigatorBearer attrs \iid -> do
            enemies <- select $ EnemyAt (locationWithInvestigator iid) <> EnemyCanBeEvadedBy (attrs.ability 1)
            chooseOneAtATimeM iid $ targets enemies $ automaticallyEvadeEnemy iid
            handleUnstableFlip iid attrs
      pure k
    _ -> TheRuinousChime <$> liftRunMessage msg attrs
