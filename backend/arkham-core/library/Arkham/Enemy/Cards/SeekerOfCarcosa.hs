module Arkham.Enemy.Cards.SeekerOfCarcosa (
  seekerOfCarcosa,
  SeekerOfCarcosa (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection

newtype SeekerOfCarcosa = SeekerOfCarcosa EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

seekerOfCarcosa :: EnemyCard SeekerOfCarcosa
seekerOfCarcosa =
  enemyWith SeekerOfCarcosa Cards.seekerOfCarcosa (2, Static 3, 2) (0, 1)
    $ spawnAtL ?~ SpawnLocation (EmptyLocation <> "Historical Society")

instance HasAbilities SeekerOfCarcosa where
  getAbilities (SeekerOfCarcosa attrs) =
    withBaseAbilities attrs
      $ [mkAbility attrs 1 $ ForcedAbility $ PhaseEnds #when #mythos]

instance RunMessage SeekerOfCarcosa where
  runMessage msg e@(SeekerOfCarcosa attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      let source = toAbilitySource attrs 1
      enemyLocation <- field EnemyLocation (toId attrs)
      for_ enemyLocation $ \loc -> do
        clueCount <- field LocationClues loc
        if clueCount > 0
          then pushAll [RemoveClues source (toTarget loc) 1, PlaceClues source (toTarget attrs) 1]
          else push $ placeDoom source attrs 1
      pure e
    _ -> SeekerOfCarcosa <$> runMessage msg attrs
