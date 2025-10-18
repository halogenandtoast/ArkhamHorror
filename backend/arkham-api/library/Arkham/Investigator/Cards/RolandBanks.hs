module Arkham.Investigator.Cards.RolandBanks (rolandBanks) where

import Arkham.Ability hiding (you)
import Arkham.Calculation
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted hiding (EnemyDefeated)
import Arkham.Location.Types
import Arkham.Matcher
import Arkham.Script

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
        $ freeReaction (IfEnemyDefeated #after You ByAny AnyEnemy)
    ]

instance HasChaosTokenValue RolandBanks where
  getChaosTokenValue iid ElderSign (RolandBanks attrs) | attrs `is` iid = do
    pure $ elderSignValue $ InvestigatorLocationFieldCalculation iid LocationClues
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage RolandBanks where
  runMessage = script $ onAbility 1 $ discoverAtYourLocation NotInvestigate you ability 1
