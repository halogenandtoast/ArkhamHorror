module Arkham.Location.Cards.GothicSet (gothicSet) where

import Arkham.Ability
import Arkham.Helpers.Investigator (canHaveHorrorHealed)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype GothicSet = GothicSet LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gothicSet :: LocationCard GothicSet
gothicSet = location GothicSet Cards.gothicSet 3 (PerPlayer 1)

instance HasAbilities GothicSet where
  getAbilities (GothicSet a) =
    extendRevealed
      a
      [ mkAbility a 1 $ forced $ DiscoverClues #after You (be a) (atLeast 1)
      , restricted a 2 (youExist $ InvestigatorWithHealableHorror (a.ability 2))
          $ freeReaction (EnemyDefeated #after You ByAny $ enemyAt a)
      ]

instance RunMessage GothicSet where
  runMessage msg l@(GothicSet attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #combat (Fixed 2)
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      chooseSelectM iid (NearestEnemyTo iid AnyEnemy) (handleTarget iid (attrs.ability 1))
      pure l
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (EnemyTarget enemy) -> do
      readyThis enemy
      chooseSelectM iid (NearestToEnemy (EnemyWithId enemy)) \iid' -> do
        enemyEngageInvestigator enemy iid'
        initiateEnemyAttack enemy (attrs.ability 1) iid'

      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      whenM (canHaveHorrorHealed (attrs.ability 2) iid) do
        healHorror iid (attrs.ability 2) 1
      pure l
    _ -> GothicSet <$> liftRunMessage msg attrs
