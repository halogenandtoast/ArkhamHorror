module Arkham.Act.Cards.StalkedByShadows (stalkedByShadows) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Campaigns.ThePathToCarcosa.Key
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.APhantomOfTruth.Helpers

newtype StalkedByShadows = StalkedByShadows ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stalkedByShadows :: ActCard StalkedByShadows
stalkedByShadows = act (2, A) StalkedByShadows Cards.stalkedByShadows Nothing

instance HasAbilities StalkedByShadows where
  getAbilities (StalkedByShadows a) =
    [groupLimit PerRound $ mkAbility a 1 $ FastAbility (GroupClueCost (PerPlayer 1) Anywhere)]

instance RunMessage StalkedByShadows where
  runMessage msg a@(StalkedByShadows attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseOneM iid do
        labeled "Place 1 doom on the current agenda" $ placeDoomOnAgenda 1
        withTheOrganist $ labeled "Automatically evade The Organist" . automaticallyEvadeEnemy iid
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      intrudedOnASecretMeeting <- getHasRecord YouIntrudedOnASecretMeeting
      push $ if intrudedOnASecretMeeting then R2 else R1
      pure a
    _ -> StalkedByShadows <$> liftRunMessage msg attrs
