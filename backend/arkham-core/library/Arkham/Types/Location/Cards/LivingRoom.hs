module Arkham.Types.Location.Cards.LivingRoom
  ( livingRoom
  , LivingRoom(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message hiding (MoveAction)
import qualified Arkham.Types.Timing as Timing

newtype LivingRoom = LivingRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

livingRoom :: LocationCard LivingRoom
livingRoom =
  location LivingRoom Cards.livingRoom 3 (Static 0) Equals [T, Circle, Plus]

instance HasAbilities LivingRoom where
  getAbilities (LivingRoom attrs) = withBaseAbilities
    attrs
    [ restrictedAbility
        attrs
        1
        Here
        (ReactionAbility
          (PerformAction Timing.After You $ ActionIs Action.Parley)
          Free
        )
      & abilityLimitL
      .~ GroupLimit PerPhase 1
    | locationRevealed attrs
    ]

instance LocationRunner env => RunMessage env LivingRoom where
  runMessage msg l@(LivingRoom attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ push (DrawCards iid 1 False)
    _ -> LivingRoom <$> runMessage msg attrs
