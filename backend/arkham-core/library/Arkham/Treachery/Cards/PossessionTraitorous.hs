module Arkham.Treachery.Cards.PossessionTraitorous (
  possessionTraitorous,
  PossessionTraitorous (..),
) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype PossessionTraitorous = PossessionTraitorous TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

possessionTraitorous :: TreacheryCard PossessionTraitorous
possessionTraitorous =
  treacheryWith
    PossessionTraitorous
    Cards.possessionTraitorous
    (canBeCommittedL .~ True)

instance RunMessage PossessionTraitorous where
  runMessage msg t@(PossessionTraitorous attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      horror <- field InvestigatorHorror iid
      sanity <- field InvestigatorSanity iid
      pushWhen (horror > sanity * 2) $ InvestigatorKilled (toSource attrs) iid
      push $ PlaceTreachery (toId attrs) (TreacheryInHandOf iid)
      pure t
    EndCheckWindow {} -> case treacheryInHandOf attrs of
      Just iid -> do
        horror <- field InvestigatorHorror iid
        sanity <- field InvestigatorSanity iid
        pushWhen (horror > sanity * 2) $ InvestigatorKilled (toSource attrs) iid
        pure t
      Nothing -> pure t
    InvestigatorCommittedCard iid card | toCardId card == toCardId attrs ->
      do
        pushAll [toDiscardBy iid attrs attrs, FailSkillTest]
        pure t
    _ -> PossessionTraitorous <$> runMessage msg attrs
