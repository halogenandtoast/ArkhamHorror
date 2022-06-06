module Arkham.Location.Cards.CanalSaintMartin
  ( canalSaintMartin
  , CanalSaintMartin(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import qualified Arkham.Location.Cards as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import qualified Arkham.Matcher as Matcher
import Arkham.Message
import Arkham.Target
import qualified Arkham.Timing as Timing

newtype CanalSaintMartin = CanalSaintMartin LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

canalSaintMartin :: LocationCard CanalSaintMartin
canalSaintMartin = location
  CanalSaintMartin
  Cards.canalSaintMartin
  4
  (PerPlayer 1)
  Equals
  [Square, T, Moon]

instance HasAbilities CanalSaintMartin where
  getAbilities (CanalSaintMartin attrs) = withBaseAbilities
    attrs
    [ limitedAbility (PlayerLimit PerRound 1)
      $ restrictedAbility attrs 1 Here
      $ ReactionAbility
          (Matcher.EnemyEvaded Timing.After You
          $ EnemyAt (LocationWithId $ toId attrs)
          )
          Free
    | locationRevealed attrs
    ]

instance LocationRunner env => RunMessage CanalSaintMartin where
  runMessage msg a@(CanalSaintMartin attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      enemies <- selectListMap EnemyTarget
        $ EnemyAt (LocationWithId $ toId attrs)
      connectingLocations <- selectList $ AccessibleFrom $ LocationWithId $ toId
        attrs
      push $ chooseOrRunOne iid $ do
        e <- enemies
        pure $ TargetLabel e $ do
          l <- connectingLocations
          pure $ chooseOrRunOne iid [targetLabel l [MoveUntil l e]]
      pure a
    _ -> CanalSaintMartin <$> runMessage msg attrs
