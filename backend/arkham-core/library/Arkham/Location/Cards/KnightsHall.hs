module Arkham.Location.Cards.KnightsHall
  ( knightsHall
  , KnightsHall(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Message
import Arkham.ScenarioLogKey
import Arkham.SkillType
import Arkham.Source

newtype KnightsHall = KnightsHall LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

knightsHall :: LocationCard KnightsHall
knightsHall = location KnightsHall Cards.knightsHall 2 (PerPlayer 1)

instance HasAbilities KnightsHall where
  getAbilities (KnightsHall a) = withBaseAbilities
    a
    [ withTooltip
        "{action} If there are no clues on Knight's Hall: _Investigate_. Investigate using {agility} instead of {intellect}. If you succeed, instead of discovering clues, remember that you have \"found the tower key.\""
      $ restrictedAbility a 1 (Here <> NoCluesOnThis)
      $ ActionAbility (Just Action.Investigate) Free
    ]

instance RunMessage KnightsHall where
  runMessage msg l@(KnightsHall attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> l <$ push
      (Investigate
        iid
        (toId attrs)
        (AbilitySource source 1)
        Nothing
        SkillAgility
        False
      )
    Successful (Action.Investigate, _) _ (AbilitySource source 1) _ _
      | isSource attrs source -> l <$ push (Remember FoundTheTowerKey)
    _ -> KnightsHall <$> runMessage msg attrs
