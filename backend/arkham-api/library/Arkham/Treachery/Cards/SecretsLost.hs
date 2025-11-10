module Arkham.Treachery.Cards.SecretsLost (secretsLost) where

import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype SecretsLost = SecretsLost TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

secretsLost :: TreacheryCard SecretsLost
secretsLost = treachery SecretsLost Cards.secretsLost

instance RunMessage SecretsLost where
  runMessage msg t@(SecretsLost attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 3)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      cards <- select $ inHandOf NotForPlay iid <> basic NonWeakness
      chooseNM iid n $ targets cards $ hollow iid
      pure t
    PassedThisSkillTestBy iid (isSource attrs -> True) n | n >= 2 -> do
      hollows <- select HollowedCard
      unless (null hollows) do
        chooseOneM iid $ campaignI18n do
          unscoped skip_
          labeled' "secretsLost.hollowed" do
            assignHorror iid attrs 1
            focusCards hollows $ chooseTargetM iid hollows \c -> for_ c.owner \owner -> addToHand owner [c]
      pure t
    _ -> SecretsLost <$> liftRunMessage msg attrs
