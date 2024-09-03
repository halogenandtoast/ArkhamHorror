module Arkham.Location.Cards.ForsakenTowerOfPrimevalLight (
  forsakenTowerOfPrimevalLight,
  ForsakenTowerOfPrimevalLight (..),
)
where

import Arkham.Attack
import Arkham.Enemy.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.WhereTheGodsDwell.Helpers

newtype ForsakenTowerOfPrimevalLight = ForsakenTowerOfPrimevalLight LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forsakenTowerOfPrimevalLight :: LocationCard ForsakenTowerOfPrimevalLight
forsakenTowerOfPrimevalLight = location ForsakenTowerOfPrimevalLight Cards.forsakenTowerOfPrimevalLight 4 (PerPlayer 1)

instance HasAbilities ForsakenTowerOfPrimevalLight where
  getAbilities (ForsakenTowerOfPrimevalLight attrs) =
    extendRevealed attrs $ forsakenTowerAbilities attrs

instance RunMessage ForsakenTowerOfPrimevalLight where
  runMessage msg l@(ForsakenTowerOfPrimevalLight attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      revealWhisperingChaos attrs
      nyarlathoteps <- select $ EnemyInHandOf $ InvestigatorWithId iid
      chooseOne
        iid
        [ targetLabel
          nyarlathotep
          [HandleTargetChoice iid (attrs.ability 1) (toTarget nyarlathotep)]
        | nyarlathotep <- nyarlathoteps
        ]
      pure l
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (EnemyTarget nyarlathotep) -> do
      sid <- getRandom
      beginSkillTest
        sid
        iid
        (attrs.ability 1)
        (toTarget nyarlathotep)
        #willpower
        (EnemyMaybeGameValueFieldCalculation nyarlathotep EnemyHealthActual)
      pure l
    PassedSkillTest _iid _ (isAbilitySource attrs 1 -> True) (Initiator target) _ _ -> do
      discardWhisperingChaos attrs
      push $ AddToVictory target
      pure l
    FailedSkillTest iid _ (isAbilitySource attrs 1 -> True) (Initiator (EnemyTarget eid)) _ _ -> do
      shuffleWhisperingChaosBackIntoEncounterDeck attrs
      pushAll
        [ InitiateEnemyAttack $ enemyAttack eid (attrs.ability 1) iid
        , ShuffleBackIntoEncounterDeck (toTarget eid)
        ]
      pure l
    _ -> ForsakenTowerOfPrimevalLight <$> liftRunMessage msg attrs
