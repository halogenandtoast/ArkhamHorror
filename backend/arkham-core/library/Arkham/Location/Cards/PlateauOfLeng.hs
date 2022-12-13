module Arkham.Location.Cards.PlateauOfLeng
  ( plateauOfLeng
  , PlateauOfLeng(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing
import Arkham.Token

newtype PlateauOfLeng = PlateauOfLeng LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

plateauOfLeng :: LocationCard PlateauOfLeng
plateauOfLeng = location PlateauOfLeng Cards.plateauOfLeng 3 (Static 1)

instance HasAbilities PlateauOfLeng where
  getAbilities (PlateauOfLeng attrs) = withBaseAbilities
    attrs
    [ restrictedAbility attrs 1 (Here <> DuringSkillTest AnySkillTest)
      $ ForcedAbility
      $ RevealChaosToken Timing.AtIf You
      $ TokenFaceIs ElderThing
    ]

instance RunMessage PlateauOfLeng where
  runMessage msg l@(PlateauOfLeng attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 1
      pure l
    _ -> PlateauOfLeng <$> runMessage msg attrs
