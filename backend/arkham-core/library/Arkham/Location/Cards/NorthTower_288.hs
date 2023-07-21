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
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

northTower_288 :: LocationCard NorthTower_288
northTower_288 = location NorthTower_288 Cards.northTower_288 4 (PerPlayer 1)

instance HasModifiersFor NorthTower_288 where
  getModifiersFor SkillTestTarget (NorthTower_288 a) = do
    mSkillTestSource <- getSkillTestSource
    case mSkillTestSource of
      Just (SkillTestSource iid _ _ _) -> do
        mAgendaA <- selectOne $ AgendaWithSide A
        mAgendaC <- selectOne $ AgendaWithSide C
        case (mAgendaA, mAgendaC) of
          (Just agendaA, Just agendaC) -> do
            aDoom <- field AgendaDoom agendaA
            cDoom <- field AgendaDoom agendaC
            let atOuterWall = iid `member` locationInvestigators a
            pure $
              toModifiers
                a
                [Difficulty (-1) | atOuterWall && aDoom > cDoom]
          _ -> pure []
      _ -> pure []
  getModifiersFor _ _ = pure []

instance RunMessage NorthTower_288 where
  runMessage msg (NorthTower_288 attrs) =
    NorthTower_288 <$> runMessage msg attrs
