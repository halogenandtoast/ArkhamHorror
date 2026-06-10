module Arkham.Treachery.Cards.Eclipse (eclipse) where

import Arkham.Campaigns.GuardiansOfTheAbyss.Helpers (assetTakenByTheAbyss, campaignI18n)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhenM)
import Arkham.I18n
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Otherworld))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Eclipse = Eclipse TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eclipse :: TreacheryCard Eclipse
eclipse = treachery Eclipse Cards.eclipse

instance HasModifiersFor Eclipse where
  getModifiersFor (Eclipse attrs) = do
    modifySelfWhenM
      attrs
      (selectAny $ locationWithInvestigator attrs.drawnBy <> LocationWithTrait Otherworld)
      [AddKeyword Keyword.Peril]

instance RunMessage Eclipse where
  runMessage msg t@(Eclipse attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      allies <- select $ #ally <> assetAtLocationWith iid
      chooseOrRunOneM iid do
        withI18n $ countVar 2 $ labeled' "takeHorror" $ assignHorror iid attrs 2
        campaignI18n $ targets allies \ally -> do
          push $ Msg.AssetDefeated (toSource attrs) ally
          assetTakenByTheAbyss ally
      pure t
    _ -> Eclipse <$> liftRunMessage msg attrs
