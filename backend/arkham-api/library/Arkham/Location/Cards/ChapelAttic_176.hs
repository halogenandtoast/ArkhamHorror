module Arkham.Location.Cards.ChapelAttic_176 (chapelAttic_176, ChapelAttic_176 (..)) where

import Arkham.Card
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest (getSkillTestInvestigator, isInvestigating)
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Import.Lifted
import Arkham.Message (ReplaceStrategy (..))
import Arkham.Projection

newtype ChapelAttic_176 = ChapelAttic_176 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

chapelAttic_176 :: LocationCard ChapelAttic_176
chapelAttic_176 = location ChapelAttic_176 Cards.chapelAttic_176 8 (Static 0)

instance HasModifiersFor ChapelAttic_176 where
  getModifiersFor (ChapelAttic_176 a) =
    getSkillTestInvestigator >>= \case
      Nothing -> pure mempty
      Just iid -> maybeModified_ a iid do
        liftGuardM $ isInvestigating iid a.id
        cardCount <- fieldMap InvestigatorHand length iid
        pure [AnySkillValue cardCount | cardCount > 0]

instance RunMessage ChapelAttic_176 where
  runMessage msg l@(ChapelAttic_176 attrs) = runQueueT $ case msg of
    Flip _ _ (isTarget attrs -> True) -> do
      spectral <- genCard Locations.chapelAtticSpectral_176
      push $ ReplaceLocation (toId attrs) spectral Swap
      pure l
    _ -> ChapelAttic_176 <$> liftRunMessage msg attrs
