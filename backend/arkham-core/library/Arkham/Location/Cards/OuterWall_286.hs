module Arkham.Location.Cards.OuterWall_286
  ( outerWall_286
  , OuterWall_286(..)
  ) where

import Arkham.Prelude

import Arkham.Agenda.Types (Field(AgendaDoom))
import Arkham.Agenda.Sequence (AgendaSide(A, C))
import Arkham.Classes
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Projection
import Arkham.Source
import Arkham.Target

newtype OuterWall_286 = OuterWall_286 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

outerWall_286 :: LocationCard OuterWall_286
outerWall_286 = location
  OuterWall_286
  Cards.outerWall_286
  4
  (PerPlayer 1)
  Triangle
  [Squiggle, Diamond, Equals]

instance HasModifiersFor OuterWall_286 where
  getModifiersFor (SkillTestSource iid _ _ _) (CardIdTarget _) (OuterWall_286 a)
    = do
      mAgendaA <- selectOne $ AgendaWithSide A
      mAgendaC <- selectOne $ AgendaWithSide C
      case (mAgendaA, mAgendaC) of
        (Just agendaA, Just agendaC) -> do
          aDoom <- field AgendaDoom agendaA
          cDoom <- field AgendaDoom agendaC
          let atOuterWall = iid `member` locationInvestigators a
          pure $ toModifiers a [ DoubleSkillIcons | atOuterWall && cDoom > aDoom ]
        _ -> pure []
  getModifiersFor _ _ _ = pure []

instance RunMessage OuterWall_286 where
  runMessage msg (OuterWall_286 attrs) = OuterWall_286 <$> runMessage msg attrs
