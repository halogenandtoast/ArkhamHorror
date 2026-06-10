module Arkham.Treachery.Cards.TerrorUnderThePyramids (terrorUnderThePyramids) where

import Arkham.Helpers.Message.Discard.Lifted (chooseAndDiscardCard)
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype TerrorUnderThePyramids = TerrorUnderThePyramids TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

terrorUnderThePyramids :: TreacheryCard TerrorUnderThePyramids
terrorUnderThePyramids = treachery TerrorUnderThePyramids Cards.terrorUnderThePyramids

instance RunMessage TerrorUnderThePyramids where
  runMessage msg t@(TerrorUnderThePyramids attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 3)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      push $ HandlePointOfFailure iid (toTarget attrs) n
      pure t
    HandlePointOfFailure _ target 0 | isTarget attrs target -> pure t
    HandlePointOfFailure iid target n | isTarget attrs target -> do
      cards <- select $ inHandOf NotForPlay iid <> basic DiscardableCard
      if null cards
        then assignHorror iid attrs 1
        else chooseAndDiscardCard iid attrs
      push $ HandlePointOfFailure iid (toTarget attrs) (n - 1)
      pure t
    _ -> TerrorUnderThePyramids <$> liftRunMessage msg attrs
