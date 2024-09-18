module Arkham.Investigator.Cards.RolandBanks (RolandBanks (..), rolandBanks) where

import Arkham.Ability
import Arkham.Calculation
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted hiding (EnemyDefeated, at)
import Arkham.Location.Types
import Arkham.Matcher
import Arkham.Script hiding (attrs)

newtype RolandBanks = RolandBanks InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

rolandBanks :: InvestigatorCard RolandBanks
rolandBanks =
  investigator RolandBanks Cards.rolandBanks
    $ Stats {health = 9, sanity = 5, willpower = 3, intellect = 3, combat = 4, agility = 2}

instance HasAbilities RolandBanks where
  getAbilities (RolandBanks attrs) =
    [ playerLimit PerRound
        $ selfAbility attrs 1 (AbleToDiscoverCluesAt YourLocation)
        $ freeReaction (EnemyDefeated #after You ByAny AnyEnemy)
    ]

instance HasChaosTokenValue RolandBanks where
  getChaosTokenValue iid ElderSign (RolandBanks attrs) | attrs `is` iid = do
    pure $ elderSignValue $ InvestigatorLocationFieldCalculation iid LocationClues
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage RolandBanks where
  runMessage = script do
    ability 1 $ discoverClues 1 yourLocation
