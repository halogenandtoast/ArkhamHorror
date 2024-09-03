module Arkham.Treachery.Cards.TorturousChords (torturousChords, TorturousChords (..)) where

import Arkham.Classes
import Arkham.Matcher (CardMatcher (AnyCard))
import Arkham.Modifier
import Arkham.Prelude
import Arkham.Token
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Helpers
import Arkham.Treachery.Runner

newtype TorturousChords = TorturousChords TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

torturousChords :: TreacheryCard TorturousChords
torturousChords = treachery TorturousChords Cards.torturousChords

instance HasModifiersFor TorturousChords where
  getModifiersFor (InvestigatorTarget iid) (TorturousChords a) | treacheryInThreatArea iid a = do
    modified a [IncreaseCostOf AnyCard 1]
  getModifiersFor _ _ = pure []

instance RunMessage TorturousChords where
  runMessage msg t@(TorturousChords attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      push $ revelationSkillTest sid iid attrs #willpower (Fixed 5)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      push $ placeInThreatArea attrs iid
      pure $ TorturousChords $ attrs & tokensL %~ setTokens Resource n
    PlayCard iid _ _ _ _ False | treacheryInThreatArea iid attrs -> do
      pushWhen (treacheryResources attrs <= 1) (toDiscardBy iid (toSource attrs) attrs)
      pure $ TorturousChords $ attrs & tokensL %~ subtractTokens Resource 1
    _ -> TorturousChords <$> runMessage msg attrs
