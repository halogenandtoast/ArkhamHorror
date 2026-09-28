module Arkham.Homebrew.CircusExMortis.Acts.AudienceParticipationVII (audienceParticipationVII) where

import Arkham.Act.Import.Lifted
import Arkham.Homebrew.CircusExMortis.Acts.AudienceParticipation
import Arkham.Homebrew.CircusExMortis.CardDefs.Acts qualified as Cards

newtype AudienceParticipationVII = AudienceParticipationVII ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

audienceParticipationVII :: ActCard AudienceParticipationVII
audienceParticipationVII = act (2, A) AudienceParticipationVII Cards.audienceParticipationVII Nothing

instance HasAbilities AudienceParticipationVII where
  getAbilities = actAbilities $ audienceParticipationAbilities (clueCost 2)

instance RunMessage AudienceParticipationVII where
  runMessage msg a@(AudienceParticipationVII attrs) = runQueueT $ case msg of
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
    _ -> AudienceParticipationVII <$> liftRunMessage msg attrs
