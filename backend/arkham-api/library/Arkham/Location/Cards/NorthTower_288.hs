module Arkham.Location.Cards.NorthTower_288 (northTower_288, NorthTower_288 (..)) where

import Arkham.Agenda.Sequence (AgendaSide (A, C))
import Arkham.Agenda.Types (Field (AgendaDoom))
import Arkham.GameValue
import Arkham.Helpers.Location (isAt)
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Projection

newtype NorthTower_288 = NorthTower_288 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

northTower_288 :: LocationCard NorthTower_288
northTower_288 = location NorthTower_288 Cards.northTower_288 4 (PerPlayer 1)

instance HasModifiersFor NorthTower_288 where
  getModifiersFor (NorthTower_288 a) = do
    getSkillTest >>= \case
      Nothing -> pure mempty
      Just st -> maybeModified_ a (SkillTestTarget st.id) do
        iid <- MaybeT getSkillTestInvestigator
        liftGuardM $ iid `isAt` a
        agendaA <- MaybeT $ selectOne $ AgendaWithSide A
        agendaC <- MaybeT $ selectOne $ AgendaWithSide C
        aDoom <- lift $ field AgendaDoom agendaA
        cDoom <- lift $ field AgendaDoom agendaC
        guard $ aDoom > cDoom
        pure [Difficulty (-1)]

instance RunMessage NorthTower_288 where
  runMessage msg (NorthTower_288 attrs) = NorthTower_288 <$> runMessage msg attrs
