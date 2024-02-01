module Arkham.Location.Cards.OuterWall_286 (
  outerWall_286,
  OuterWall_286 (..),
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

newtype OuterWall_286 = OuterWall_286 LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, HasAbilities)

outerWall_286 :: LocationCard OuterWall_286
outerWall_286 = location OuterWall_286 Cards.outerWall_286 4 (PerPlayer 1)

instance HasModifiersFor OuterWall_286 where
  getModifiersFor (CardIdTarget _) (OuterWall_286 a) = do
    mMod <- runMaybeT $ do
      iid <- MaybeT getSkillTestInvestigator
      agendaA <- MaybeT $ selectOne $ AgendaWithSide A
      agendaC <- MaybeT $ selectOne $ AgendaWithSide C
      aDoom <- lift $ field AgendaDoom agendaA
      cDoom <- lift $ field AgendaDoom agendaC
      here <- lift $ iid `isAt` a
      guard $ here && cDoom > aDoom
      pure $ DoubleSkillIcons
    pure $ toModifiers a $ maybeToList mMod
  getModifiersFor _ _ = pure []

instance RunMessage OuterWall_286 where
  runMessage msg (OuterWall_286 attrs) = OuterWall_286 <$> runMessage msg attrs
