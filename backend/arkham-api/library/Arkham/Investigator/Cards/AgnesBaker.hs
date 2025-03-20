module Arkham.Investigator.Cards.AgnesBaker (agnesBaker) where

import Arkham.Ability hiding (atYourLocation, you)
import Arkham.Calculation
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher hiding (NonAttackDamageEffect)
import Arkham.Script

newtype AgnesBaker = AgnesBaker InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

agnesBaker :: InvestigatorCard AgnesBaker
agnesBaker =
  investigator AgnesBaker Cards.agnesBaker
    $ Stats {health = 6, sanity = 8, willpower = 5, intellect = 2, combat = 2, agility = 3}

instance HasAbilities AgnesBaker where
  getAbilities (AgnesBaker x) =
    [ playerLimit PerPhase
        $ restricted x 1 (Self <> CanDealDamage <> exists (EnemyAt YourLocation))
        $ freeReaction (PlacedCounter #after You AnySource #horror $ atLeast 1)
    ]

instance HasChaosTokenValue AgnesBaker where
  getChaosTokenValue iid ElderSign (AgnesBaker attrs) | attrs `is` iid = do
    pure $ elderSignValue $ InvestigatorFieldCalculation attrs.id InvestigatorHorror
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage AgnesBaker where
  runMessage = script $ onAbility 1 do
    chooseEnemy (atYourLocation <> ability.canDamage) $ nonAttackEnemyDamage ability 1
