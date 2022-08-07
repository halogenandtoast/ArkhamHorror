module Arkham.Treachery.Cards.PossessionTraitorous
  ( possessionTraitorous
  , PossessionTraitorous(..)
  ) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Id
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Message
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype PossessionTraitorous = PossessionTraitorous TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

possessionTraitorous :: TreacheryCard PossessionTraitorous
possessionTraitorous = treacheryWith
  PossessionTraitorous
  Cards.possessionTraitorous
  (canBeCommittedL .~ True)

instance RunMessage PossessionTraitorous where
  runMessage msg t@(PossessionTraitorous attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      horror <- field InvestigatorHorror iid
      sanity <- field InvestigatorSanity iid
      when (horror > sanity * 2) $ push $ InvestigatorKilled (toSource attrs) iid
      t <$ push (AddTreacheryToHand iid (toId attrs))
    EndCheckWindow {} -> case treacheryInHandOf attrs of
      Just iid -> do
        horror <- field InvestigatorHorror iid
        sanity <- field InvestigatorSanity iid
        when (horror > sanity * 2) $ push $ InvestigatorKilled (toSource attrs) iid
        pure t
      Nothing -> pure t
    InvestigatorCommittedCard _ card | toCardId card == unTreacheryId (toId attrs) ->
      do
        pushAll [Discard (toTarget attrs), FailSkillTest]
        pure t
    _ -> PossessionTraitorous <$> runMessage msg attrs
