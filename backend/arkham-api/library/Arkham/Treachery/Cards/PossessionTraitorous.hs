module Arkham.Treachery.Cards.PossessionTraitorous (possessionTraitorous) where

import Arkham.Card
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Helpers.SkillTest (getSkillTest)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher hiding (treacheryInHandOf)
import Arkham.Placement
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype PossessionTraitorous = PossessionTraitorous TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

possessionTraitorous :: TreacheryCard PossessionTraitorous
possessionTraitorous = treacheryWith PossessionTraitorous Cards.possessionTraitorous (canBeCommittedL .~ True)

instance HasModifiersFor PossessionTraitorous where
  getModifiersFor (PossessionTraitorous attrs) = do
    case attrs.placement of
      HiddenInHand iid -> do
        whenJustM getSkillTest \st -> do
          allCommittedCards <- map (.id) <$> select (CardIsCommittedBy $ InvestigatorWithId iid)
          when (toCardId attrs `elem` allCommittedCards) do
            modified_ attrs st [SkillTestAutomaticallyFails]
      _ -> pure ()

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
    Do (SkillTestEnds {}) -> do
      case attrs.placement of
        HiddenInHand iid -> do
          allCommittedCards <- map (.id) <$> select (CardIsCommittedBy $ InvestigatorWithId iid)
          when (toCardId attrs `elem` allCommittedCards) do
            toDiscardBy iid attrs attrs
        _ -> pure ()
      pure t
    _ -> PossessionTraitorous <$> liftRunMessage msg attrs
