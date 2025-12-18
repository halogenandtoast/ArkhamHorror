module Arkham.Campaigns.TheScarletKeys.Key.Cards.TheTwistedAntiprism (theTwistedAntiprism) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Key.Cards qualified as Cards
import Arkham.Campaigns.TheScarletKeys.Key.Import.Lifted
import Arkham.Capability
import Arkham.Helpers.Location (placementLocation)
import Arkham.Helpers.Query (getLead)
import Arkham.Matcher hiding (key)
import Arkham.Message.Lifted.Choose
import Arkham.Strategy

newtype TheTwistedAntiprism = TheTwistedAntiprism ScarletKeyAttrs
  deriving anyclass (IsScarletKey, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTwistedAntiprism :: ScarletKeyCard TheTwistedAntiprism
theTwistedAntiprism = key TheTwistedAntiprism Cards.theTwistedAntiprism

instance HasAbilities TheTwistedAntiprism where
  getAbilities (TheTwistedAntiprism a) = case a.bearer of
    InvestigatorTarget iid | not a.shifted ->
      case a.stability of
        Stable ->
          [ restricted a 1 (exists (colocatedWith iid <> can.manipulate.deck)) $ FastAbility Free
          ]
        Unstable -> [restricted a 1 (youExist (InvestigatorWithId iid)) $ FastAbility Free]
    _ -> []

instance RunMessage TheTwistedAntiprism where
  runMessage msg k@(TheTwistedAntiprism attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> liftRunMessage (CampaignSpecific "shift[09590]" Null) k
    CampaignSpecific "shift[09590]" _ -> do
      shiftKey attrs do
        when attrs.unstable do
          placementLocation attrs.placement >>= traverse_ \lid -> do
            sameLocation <- select (InvestigatorAt $ LocationWithId lid)
            investigators <- sameLocation `orWhenNull` select (NearestToLocation $ LocationWithId lid)
            lead <- getLead
            chooseOneM lead $ targets investigators $ handleTarget lead attrs
        when attrs.stable do
          withInvestigatorBearer attrs \iid -> do
            lookAt iid attrs iid [fromTopOfDeck 3] (basic AnyCard) (DrawFound iid 1)
            flipOver iid attrs
      pure k
    HandleTargetChoice _ (isSource attrs -> True) (InvestigatorTarget iid) -> do
      assets <- select $ assetControlledBy iid
      locations <- select $ locationWithInvestigator iid
      enemies <- select $ EnemyAt (locationWithInvestigator iid)
      treacheries <- select $ TreacheryAt (locationWithInvestigator iid)
      agendas <- select AnyAgenda

      chooseOneM iid do
        targets assets $ placeDoomOn attrs 1
        targets locations $ placeDoomOn attrs 1
        targets enemies $ placeDoomOn attrs 1
        targets treacheries $ placeDoomOn attrs 1
        targets agendas $ placeDoomOn attrs 1

      withInvestigatorBearer attrs (`handleUnstableFlip` attrs)
      pure k
    _ -> TheTwistedAntiprism <$> liftRunMessage msg attrs
