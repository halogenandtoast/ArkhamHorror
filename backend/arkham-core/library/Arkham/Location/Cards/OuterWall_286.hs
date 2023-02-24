module Arkham.Location.Cards.OuterWall_286
  ( outerWall_286
  , OuterWall_286(..)
  ) where

import Arkham.Prelude

import Arkham.Agenda.Sequence ( AgendaSide (A, C) )
import Arkham.Agenda.Types ( Field (AgendaDoom) )
import Arkham.Classes
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Projection
import Arkham.Source

newtype OuterWall_286 = OuterWall_286 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

outerWall_286 :: LocationCard OuterWall_286
outerWall_286 = location OuterWall_286 Cards.outerWall_286 4 (PerPlayer 1)

instance HasModifiersFor OuterWall_286 where
  getModifiersFor (CardIdTarget _) (OuterWall_286 a) = do
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
            pure $ toModifiers
              a
              [ DoubleSkillIcons | atOuterWall && cDoom > aDoom ]
          _ -> pure []
      _ -> pure []
  getModifiersFor _ _ = pure []

instance RunMessage OuterWall_286 where
  runMessage msg (OuterWall_286 attrs) = OuterWall_286 <$> runMessage msg attrs
