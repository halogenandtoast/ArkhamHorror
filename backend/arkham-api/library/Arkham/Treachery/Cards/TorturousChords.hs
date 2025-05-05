module Arkham.Treachery.Cards.TorturousChords (torturousChords) where

import Arkham.Helpers.Modifiers (ModifierType (..), inThreatAreaGets)
import Arkham.Matcher (CardMatcher (AnyCard), basic)
import Arkham.Token
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype TorturousChords = TorturousChords TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

torturousChords :: TreacheryCard TorturousChords
torturousChords = treachery TorturousChords Cards.torturousChords

instance HasModifiersFor TorturousChords where
  getModifiersFor (TorturousChords a) =
    inThreatAreaGets a [IncreaseCostOf (basic AnyCard) 1]

instance RunMessage TorturousChords where
  runMessage msg t@(TorturousChords attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 5)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      placeInThreatArea attrs iid
      pure $ TorturousChords $ attrs & tokensL %~ setTokens Resource n
    PlayCard iid _ _ _ _ False | treacheryInThreatArea iid attrs -> do
      when (attrs.resources <= 1) (priority $ toDiscardBy iid attrs attrs)
      pure $ TorturousChords $ attrs & tokensL %~ subtractTokens Resource 1
    _ -> TorturousChords <$> liftRunMessage msg attrs
