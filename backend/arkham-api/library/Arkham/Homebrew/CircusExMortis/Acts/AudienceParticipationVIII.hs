module Arkham.Homebrew.CircusExMortis.Acts.AudienceParticipationVIII (audienceParticipationVIII) where

import Arkham.Act.Import.Lifted
import Arkham.Homebrew.CircusExMortis.Acts.AudienceParticipation
import Arkham.Homebrew.CircusExMortis.CardDefs.Acts qualified as Cards

newtype AudienceParticipationVIII = AudienceParticipationVIII ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

audienceParticipationVIII :: ActCard AudienceParticipationVIII
audienceParticipationVIII = act (2, A) AudienceParticipationVIII Cards.audienceParticipationVIII Nothing

instance HasAbilities AudienceParticipationVIII where
  getAbilities = actAbilities $ audienceParticipationAbilities (clueCost 2 <> HandDiscardCost 1 #any)

instance RunMessage AudienceParticipationVIII where
  runMessage msg a@(AudienceParticipationVIII attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 ws _ -> do
      audienceParticipationSealReleased iid ws
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      audienceParticipationSeal iid
      pure a
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      audienceParticipationAdvance attrs
      pure a
    _ -> AudienceParticipationVIII <$> liftRunMessage msg attrs
