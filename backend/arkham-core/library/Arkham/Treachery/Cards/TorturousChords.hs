module Arkham.Treachery.Cards.TorturousChords
  ( torturousChords
  , TorturousChords(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Matcher ( CardMatcher (AnyCard) )
import Arkham.Message
import Arkham.Modifier
import Arkham.SkillType
import Arkham.Target
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Helpers
import Arkham.Treachery.Runner

newtype TorturousChords = TorturousChords TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

torturousChords :: TreacheryCard TorturousChords
torturousChords = treachery TorturousChords Cards.torturousChords

instance HasModifiersFor TorturousChords where
  getModifiersFor _ target (TorturousChords a) | treacheryOn target a =
    pure $ toModifiers a [IncreaseCostOf AnyCard 1]
  getModifiersFor _ _ _ = pure []

instance RunMessage TorturousChords where
  runMessage msg t@(TorturousChords attrs) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ pushAll [RevelationSkillTest iid source SkillWillpower 5]
    FailedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget{} _ n
      -> do
        push $ AttachTreachery (toId attrs) (InvestigatorTarget iid)
        pure $ TorturousChords $ attrs & resourcesL .~ n
    PassedSkillTest _ _ (isSource attrs -> True) SkillTestInitiatorTarget{} _ _
      -> do
        push $ Discard $ toTarget attrs
        pure t
    PlayCard iid _ _ _ False | treacheryOnInvestigator iid attrs -> do
      when
        (treacheryResources attrs <= 1)
        (push $ Discard $ toTarget attrs)
      pure $ TorturousChords $ attrs & resourcesL %~ (max 0 . subtract 1)
    _ -> TorturousChords <$> runMessage msg attrs
