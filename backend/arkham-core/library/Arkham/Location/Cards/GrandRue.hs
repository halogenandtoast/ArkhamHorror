module Arkham.Location.Cards.GrandRue (
  grandRue,
  GrandRue (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Types (Field (AgendaDoom))
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype GrandRue = GrandRue LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

grandRue :: LocationCard GrandRue
grandRue = location GrandRue Cards.grandRue 1 (PerPlayer 1)

instance HasAbilities GrandRue where
  getAbilities (GrandRue a) =
    withRevealedAbilities
      a
      [ restrictedAbility a 1 Here
          $ ForcedAbility
          $ SkillTestResult Timing.After You AnySkillTest
          $ SuccessResult
          $ AtMost
          $ Static 1
      ]

instance RunMessage GrandRue where
  runMessage msg l@(GrandRue attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      maxDoom <- fieldMax AgendaDoom AnyAgenda
      agendas <-
        selectListMap AgendaTarget
          $ AgendaWithDoom
          $ EqualTo
          $ Static
            maxDoom
      player <- getPlayer iid
      push
        $ chooseOrRunOne
          player
          [targetLabel target [PlaceDoom (toAbilitySource attrs 1) target 1] | target <- agendas]
      pure l
    _ -> GrandRue <$> runMessage msg attrs
