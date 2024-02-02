module Arkham.Enemy.Cards.DevoteeOfTheKey (
  devoteeOfTheKey,
  DevoteeOfTheKey (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Projection

newtype DevoteeOfTheKey = DevoteeOfTheKey EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

devoteeOfTheKey :: EnemyCard DevoteeOfTheKey
devoteeOfTheKey =
  enemyWith DevoteeOfTheKey Cards.devoteeOfTheKey (3, Static 3, 3) (1, 1)
    $ spawnAtL
    ?~ "Base of the Hill"

instance HasAbilities DevoteeOfTheKey where
  getAbilities (DevoteeOfTheKey attrs) =
    withBaseAbilities attrs [mkAbility attrs 1 $ ForcedAbility $ PhaseEnds #when #enemy]

instance RunMessage DevoteeOfTheKey where
  runMessage msg e@(DevoteeOfTheKey attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      enemyLocation <- field EnemyLocation (toId attrs)
      for_ enemyLocation \loc -> do
        sentinelPeak <- selectJust (LocationWithTitle "Sentinel Peak")
        if loc == sentinelPeak
          then
            pushAll
              [toDiscard (toAbilitySource attrs 1) attrs, PlaceDoomOnAgenda, PlaceDoomOnAgenda]
          else do
            lead <- getLeadPlayer
            choices <- selectList $ ClosestPathLocation loc sentinelPeak
            case choices of
              [] -> error "should not happen"
              xs -> push $ chooseOrRunOne lead $ targetLabels xs (only . EnemyMove (toId attrs))
      pure e
    _ -> DevoteeOfTheKey <$> runMessage msg attrs
