module Arkham.Treachery.Cards.TorturousChords (
  torturousChords,
  TorturousChords (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Matcher (CardMatcher (AnyCard))
import Arkham.Modifier
import Arkham.Token
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Helpers
import Arkham.Treachery.Runner

newtype TorturousChords = TorturousChords TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

torturousChords :: TreacheryCard TorturousChords
torturousChords = treachery TorturousChords Cards.torturousChords

instance HasModifiersFor TorturousChords where
  getModifiersFor target (TorturousChords a)
    | treacheryOn target a =
        pure $ toModifiers a [IncreaseCostOf AnyCard 1]
  getModifiersFor _ _ = pure []

instance RunMessage TorturousChords where
  runMessage msg t@(TorturousChords attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      t <$ pushAll [RevelationSkillTest iid source #willpower 5]
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      push $ attachTreachery attrs iid
      pure $ TorturousChords $ attrs & tokensL %~ setTokens Resource n
    PlayCard iid _ _ _ False | treacheryOnInvestigator iid attrs -> do
      pushWhen
        (treacheryResources attrs <= 1)
        (toDiscardBy iid (toSource attrs) attrs)
      pure $ TorturousChords $ attrs & tokensL %~ subtractTokens Resource 1
    _ -> TorturousChords <$> runMessage msg attrs
