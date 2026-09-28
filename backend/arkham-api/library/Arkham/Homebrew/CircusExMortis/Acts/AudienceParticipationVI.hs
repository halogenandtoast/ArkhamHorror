module Arkham.Homebrew.CircusExMortis.Acts.AudienceParticipationVI (audienceParticipationVI) where

import Arkham.Act.Import.Lifted
import Arkham.Homebrew.CircusExMortis.Acts.AudienceParticipation
import Arkham.Homebrew.CircusExMortis.CardDefs.Acts qualified as Cards

newtype AudienceParticipationVI = AudienceParticipationVI ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

audienceParticipationVI :: ActCard AudienceParticipationVI
audienceParticipationVI = act (2, A) AudienceParticipationVI Cards.audienceParticipationVI Nothing

instance HasAbilities AudienceParticipationVI where
  getAbilities = actAbilities $ audienceParticipationAbilities (clueCost 1)

instance RunMessage AudienceParticipationVI where
  runMessage msg a@(AudienceParticipationVI attrs) = runQueueT $ case msg of
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
    _ -> AudienceParticipationVI <$> liftRunMessage msg attrs
