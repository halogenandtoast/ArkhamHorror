module Arkham.Investigator.Cards.RolandBanks (
  RolandBanks (..),
  rolandBanks,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Location.Types
import Arkham.Matcher
import Arkham.Message hiding (EnemyDefeated)
import Arkham.Projection
import Arkham.Timing qualified as Timing

newtype RolandBanks = RolandBanks InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rolandBanks :: InvestigatorCard RolandBanks
rolandBanks =
  investigator RolandBanks Cards.rolandBanks
    $ Stats {health = 9, sanity = 5, willpower = 3, intellect = 3, combat = 4, agility = 2}

instance HasAbilities RolandBanks where
  getAbilities (RolandBanks attrs) =
    [ playerLimit PerRound
        $ reaction attrs 1 (OnLocation LocationWithAnyClues <> CanDiscoverCluesAt YourLocation) Free
        $ EnemyDefeated Timing.After You ByAny AnyEnemy
    ]

instance HasChaosTokenValue RolandBanks where
  getChaosTokenValue iid ElderSign (RolandBanks attrs) | attrs `is` iid = do
    clues <- field LocationClues attrs.location
    pure $ ChaosTokenValue ElderSign (PositiveModifier clues)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage RolandBanks where
  runMessage msg i@(RolandBanks attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ InvestigatorDiscoverCluesAtTheirLocation iid (toAbilitySource attrs 1) 1 Nothing
      pure i
    _ -> RolandBanks <$> runMessage msg attrs
