module Arkham.Enemy.Cards.WingedOneFogOverInnsmouth
  ( wingedOneFogOverInnsmouth
  , WingedOneFogOverInnsmouth(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype WingedOneFogOverInnsmouth = WingedOneFogOverInnsmouth EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

wingedOneFogOverInnsmouth :: EnemyCard WingedOneFogOverInnsmouth
wingedOneFogOverInnsmouth = enemy WingedOneFogOverInnsmouth Cards.wingedOneFogOverInnsmouth (3, Static 5, 0) (1, 1)

instance RunMessage WingedOneFogOverInnsmouth where
  runMessage msg (WingedOneFogOverInnsmouth attrs) =
    WingedOneFogOverInnsmouth <$> runMessage msg attrs
