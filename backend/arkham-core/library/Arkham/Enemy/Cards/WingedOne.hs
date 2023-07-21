module Arkham.Enemy.Cards.WingedOne (
  wingedOne,
  WingedOne (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing
import Arkham.Window
import Arkham.Window qualified as Window

newtype WingedOne = WingedOne EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wingedOne :: EnemyCard WingedOne
wingedOne = enemy WingedOne Cards.wingedOne (3, Static 3, 4) (3, 1)

instance HasAbilities WingedOne where
  getAbilities (WingedOne a) =
    withBaseAbilities
      a
      [ restrictedAbility
          a
          1
          ( EnemyCriteria $
              EnemyExists $
                EnemyWithId (toId a)
                  <> ReadyEnemy
                  <> UnengagedEnemy
          )
          $ ForcedAbility
          $ Matcher.FlipLocation Timing.When Anyone Anywhere
      ]

instance RunMessage WingedOne where
  runMessage msg e@(WingedOne attrs) = case msg of
    UseCardAbility _ source 1 [Window _ (Window.FlipLocation _ lid)] _
      | isSource attrs source -> do
          push $ MoveToward (toTarget attrs) (LocationWithId lid)
          pure e
    _ -> WingedOne <$> runMessage msg attrs
