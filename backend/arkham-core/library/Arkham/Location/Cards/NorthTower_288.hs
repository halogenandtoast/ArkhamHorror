module Arkham.Location.Cards.NorthTower_288 (
  northTower_288,
  NorthTower_288 (..),
) where

import Arkham.Prelude

import Arkham.Agenda.Sequence (AgendaSide (A, C))
import Arkham.Agenda.Types (Field (AgendaDoom))
import Arkham.Classes
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Projection

newtype NorthTower_288 = NorthTower_288 LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData, HasAbilities)

northTower_288 :: LocationCard NorthTower_288
northTower_288 = location NorthTower_288 Cards.northTower_288 4 (PerPlayer 1)

instance HasModifiersFor NorthTower_288 where
  getModifiersFor SkillTestTarget (NorthTower_288 a) = do
    mMod <- runMaybeT $ do
      iid <- MaybeT getSkillTestInvestigator
      agendaA <- MaybeT $ selectOne $ AgendaWithSide A
      agendaC <- MaybeT $ selectOne $ AgendaWithSide C
      aDoom <- lift $ field AgendaDoom agendaA
      cDoom <- lift $ field AgendaDoom agendaC
      here <- lift $ iid `isAt` a
      guard $ here && aDoom > cDoom
      pure $ Difficulty (-1)
    pure $ toModifiers a $ toList mMod
  getModifiersFor _ _ = pure []

instance RunMessage NorthTower_288 where
  runMessage msg (NorthTower_288 attrs) = NorthTower_288 <$> runMessage msg attrs
