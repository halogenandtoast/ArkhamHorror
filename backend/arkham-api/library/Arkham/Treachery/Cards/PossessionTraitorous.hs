module Arkham.Treachery.Cards.PossessionTraitorous (
  possessionTraitorous,
  PossessionTraitorous (..),
) where

import Arkham.Card
import Arkham.Investigator.Types (Field (..))
import Arkham.Placement
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype PossessionTraitorous = PossessionTraitorous TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

possessionTraitorous :: TreacheryCard PossessionTraitorous
possessionTraitorous = treacheryWith PossessionTraitorous Cards.possessionTraitorous (canBeCommittedL .~ True)

instance RunMessage PossessionTraitorous where
  runMessage msg t@(PossessionTraitorous attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      horror <- field InvestigatorHorror iid
      sanity <- field InvestigatorSanity iid
      when (horror > sanity * 2) $ kill attrs iid
      placeTreachery attrs (HiddenInHand iid)
      pure t
    EndCheckWindow {} -> case treacheryInHandOf attrs of
      Just iid -> do
        horror <- field InvestigatorHorror iid
        sanity <- field InvestigatorSanity iid
        when (horror > sanity * 2) $ kill attrs iid
        pure t
      Nothing -> pure t
    InvestigatorCommittedCard iid card | toCardId card == toCardId attrs -> do
      toDiscardBy iid attrs attrs
      failSkillTest
      pure t
    _ -> PossessionTraitorous <$> liftRunMessage msg attrs
