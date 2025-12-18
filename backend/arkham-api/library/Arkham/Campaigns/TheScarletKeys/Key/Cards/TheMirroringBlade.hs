module Arkham.Campaigns.TheScarletKeys.Key.Cards.TheMirroringBlade (theMirroringBlade) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Campaigns.TheScarletKeys.Key.Cards qualified as Cards
import Arkham.Campaigns.TheScarletKeys.Key.Import.Lifted
import Arkham.Matcher hiding (key)
import Arkham.Message.Lifted.Choose

newtype TheMirroringBlade = TheMirroringBlade ScarletKeyAttrs
  deriving anyclass (IsScarletKey, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theMirroringBlade :: ScarletKeyCard TheMirroringBlade
theMirroringBlade = key TheMirroringBlade Cards.theMirroringBlade

instance HasAbilities TheMirroringBlade where
  getAbilities (TheMirroringBlade a) = case a.bearer of
    InvestigatorTarget iid | not a.shifted ->
      case a.stability of
        Stable ->
          [ restricted
              a
              1
              (exists $ EnemyAt (locationWithInvestigator iid) <> EnemyCanBeDamagedBySource (a.ability 1))
              $ FastAbility Free
          ]
        Unstable -> [restricted a 1 (youExist (InvestigatorWithId iid)) $ FastAbility Free]
    _ -> []

instance RunMessage TheMirroringBlade where
  runMessage msg k@(TheMirroringBlade attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> liftRunMessage (CampaignSpecific "shift[09768]" Null) k
    CampaignSpecific "shift[09768]" _ -> do
      shiftKey attrs do
        when attrs.unstable do
          eachInvestigator \iid -> directDamage iid (attrs.ability 1) 1
          withInvestigatorBearer attrs (`flipOver` attrs)
        when attrs.stable do
          withInvestigatorBearer attrs \iid -> do
            enemies <-
              select $ EnemyAt (locationWithInvestigator iid) <> EnemyCanBeDamagedBySource (attrs.ability 1)
            chooseOneM iid $ campaignI18n do
              labeled' "theMirroringBlade.single" do
                chooseTargetM iid enemies $ nonAttackEnemyDamage (Just iid) (attrs.ability 1) 2
              labeled' "theMirroringBlade.all" do
                chooseOneAtATimeM iid do
                  targets enemies $ nonAttackEnemyDamage (Just iid) (attrs.ability 1) 1
            handleUnstableFlip iid attrs

      pure k
    _ -> TheMirroringBlade <$> liftRunMessage msg attrs
