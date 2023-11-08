module Arkham.Location.Cards.KnightsHall (
  knightsHall,
  KnightsHall (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.GameValue
import Arkham.Investigate
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.ScenarioLogKey

newtype KnightsHall = KnightsHall LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

knightsHall :: LocationCard KnightsHall
knightsHall = location KnightsHall Cards.knightsHall 2 (PerPlayer 1)

instance HasAbilities KnightsHall where
  getAbilities (KnightsHall a) =
    withBaseAbilities a
      $ [ withTooltip
            "{action} If there are no clues on Knight's Hall: _Investigate_. Investigate using {agility} instead of {intellect}. If you succeed, instead of discovering clues, remember that you have \"found the tower key.\""
            $ restrictedAbility a 1 (Here <> NoCluesOnThis)
            $ ActionAbility [Action.Investigate]
            $ ActionCost 1
        ]

instance RunMessage KnightsHall where
  runMessage msg l@(KnightsHall attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      pushM $ mkInvestigate iid (toAbilitySource attrs 1) <&> withSkillType #agility
      pure l
    Successful (Action.Investigate, _) _ (AbilitySource source 1) _ _ | isSource attrs source -> do
      push $ Remember FoundTheTowerKey
      pure l
    _ -> KnightsHall <$> runMessage msg attrs
