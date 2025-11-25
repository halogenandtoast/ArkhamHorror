module Arkham.Enemy.Cards.TheRedGlovedManPurposeUnknown (theRedGlovedManPurposeUnknown) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Campaigns.TheScarletKeys.Key.Matcher
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement

newtype TheRedGlovedManPurposeUnknown = TheRedGlovedManPurposeUnknown EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theRedGlovedManPurposeUnknown :: EnemyCard TheRedGlovedManPurposeUnknown
theRedGlovedManPurposeUnknown =
  enemy
    TheRedGlovedManPurposeUnknown
    Cards.theRedGlovedManPurposeUnknown
    (5, Static 3, 5)
    (1, 1)

instance HasAbilities TheRedGlovedManPurposeUnknown where
  getAbilities (TheRedGlovedManPurposeUnknown a) =
    [ restricted a 1 (thisExists a $ oneOf [EnemyWithAnyScarletKey, EnemyCanAttack You])
        $ forced
        $ CampaignEvent #after (Just You) "stabilizedKey"
    ]

instance RunMessage TheRedGlovedManPurposeUnknown where
  runMessage msg e@(TheRedGlovedManPurposeUnknown attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      initiateEnemyAttack attrs (attrs.ability 1) iid
      skeys <- select $ ScarletKeyWithPlacement (AttachedToEnemy attrs.id)
      chooseOneAtATimeM iid $ targets skeys shift
      pure e
    _ -> TheRedGlovedManPurposeUnknown <$> liftRunMessage msg attrs
