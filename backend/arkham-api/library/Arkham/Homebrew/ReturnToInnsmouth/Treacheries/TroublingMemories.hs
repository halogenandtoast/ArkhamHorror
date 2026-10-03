module Arkham.Homebrew.ReturnToInnsmouth.Treacheries.TroublingMemories (troublingMemories) where

import Arkham.Campaigns.TheInnsmouthConspiracy.Key
import Arkham.Helpers.Log (getRecordSet)
import Arkham.Helpers.Message.Discard.Lifted (chooseAndDiscardCard)
import Arkham.Homebrew.ReturnToInnsmouth.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.ReturnToInnsmouth.Helpers (campaignI18n)
import Arkham.I18n
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Import.Lifted

newtype TroublingMemories = TroublingMemories TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

troublingMemories :: TreacheryCard TroublingMemories
troublingMemories = treachery TroublingMemories Cards.troublingMemories

data Option = OptHorror | OptDamage | OptDiscard | OptSurge
  deriving stock (Eq, Enum, Bounded)

{- | "For each entry under 'Memories Recovered,' you must choose a different option."
Four options and four memories available in Pit of Despair, so late in the scenario the
choice can run out and every remaining option is forced (designer's FAQ).
-}
instance RunMessage TroublingMemories where
  runMessage msg t@(TroublingMemories attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      memories <- length <$> getRecordSet MemoriesRecovered
      chooseDistinct iid attrs (min memories 4) [minBound .. maxBound]
      pure t
    _ -> TroublingMemories <$> liftRunMessage msg attrs

chooseDistinct :: ReverseQueue m => InvestigatorId -> TreacheryAttrs -> Int -> [Option] -> m ()
chooseDistinct _ _ 0 _ = pure ()
chooseDistinct _ _ _ [] = pure ()
chooseDistinct iid attrs n options =
  chooseOneM iid $ campaignI18n $ scope "troublingMemories" $ for_ options \option ->
    labeled (labelFor option) do
      resolve option
      chooseDistinct iid attrs (n - 1) (filter (/= option) options)
 where
  labelFor = \case
    OptHorror -> "takeHorror"
    OptDamage -> "takeDamage"
    OptDiscard -> "discardCard"
    OptSurge -> "gainSurge"
  resolve = \case
    OptHorror -> assignHorror iid attrs 1
    OptDamage -> assignDamage iid attrs 1
    OptDiscard -> chooseAndDiscardCard iid attrs
    OptSurge -> gainSurge attrs
