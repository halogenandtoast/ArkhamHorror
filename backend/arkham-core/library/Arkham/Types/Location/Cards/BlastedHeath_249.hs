module Arkham.Types.Location.Cards.BlastedHeath_249
  ( blastedHeath_249
  , BlastedHeath_249(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (blastedHeath_249)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Criteria
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message
import qualified Arkham.Types.Timing as Timing

newtype BlastedHeath_249 = BlastedHeath_249 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blastedHeath_249 :: LocationCard BlastedHeath_249
blastedHeath_249 = location
  BlastedHeath_249
  Cards.blastedHeath_249
  3
  (Static 2)
  Square
  [Circle, Hourglass]

instance HasAbilities env BlastedHeath_249 where
  getAbilities iid window (BlastedHeath_249 attrs) =
    withBaseAbilities iid window attrs $ pure
      [ restrictedAbility attrs 1 Here $ ForcedAbility $ TurnEnds
          Timing.When
          You
      | locationRevealed attrs
      ]

instance LocationRunner env => RunMessage env BlastedHeath_249 where
  runMessage msg l@(BlastedHeath_249 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ push (InvestigatorAssignDamage iid source DamageAny 1 0)
    _ -> BlastedHeath_249 <$> runMessage msg attrs
