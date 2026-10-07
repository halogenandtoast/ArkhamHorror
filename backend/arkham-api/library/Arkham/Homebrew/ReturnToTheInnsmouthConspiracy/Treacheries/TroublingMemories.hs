module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Treacheries.TroublingMemories (troublingMemories) where

import Arkham.Campaigns.TheInnsmouthConspiracy.Key
import Arkham.Helpers.Log (getRecordSet)
import Arkham.Helpers.Message.Discard.Lifted (chooseAndDiscardCard)
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Helpers (campaignI18n)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Import.Lifted

newtype TroublingMemories = TroublingMemories TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

troublingMemories :: TreacheryCard TroublingMemories
troublingMemories = treachery TroublingMemories Cards.troublingMemories

data Option = OptHorror | OptDamage | OptDiscard | OptSurge
  deriving stock (Eq, Enum, Bounded)
  deriving (ToJSON, FromJSON) via Enumerated Option

instance RunMessage TroublingMemories where
  runMessage msg t@(TroublingMemories attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      n <- length <$> getRecordSet MemoriesRecovered
      doStep n msg
      pure t
    DoStep n (Revelation iid (isSource attrs -> True)) | n > 0 -> campaignI18n $ scope "troublingMemories" do
      let
        chosen = toResultDefault [] attrs.meta
        isValid OptDiscard = matches iid (HandWith $ HasCard DiscardableCard)
        isValid _ = pure True
        handleOption opt lbl body = unless (opt `elem` chosen) do
          whenM (isValid opt) do
            labeled lbl $ body >> forChoice n msg >> doNextStep msg
      chooseOneM iid do
        handleOption OptHorror "takeHorror" $ assignHorror iid attrs 1
        handleOption OptDamage "takeDamage" $ assignDamage iid attrs 1
        handleOption OptDiscard "discardCard" $ chooseAndDiscardCard iid attrs
        handleOption OptSurge "gainSurge" $ gainSurge attrs
      pure t
    ForChoice n (Revelation _iid (isSource attrs -> True)) -> do
      let mopt = toEnumMaybe @Option n
      let chosen = toResultDefault [] attrs.meta
      pure $ maybe t (\opt -> t & setMeta (opt : chosen)) mopt
    _ -> TroublingMemories <$> liftRunMessage msg attrs
