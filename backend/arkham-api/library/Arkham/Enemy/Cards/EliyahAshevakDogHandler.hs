module Arkham.Enemy.Cards.EliyahAshevakDogHandler (eliyahAshevakDogHandler) where

import Arkham.Ability
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyDefeated)
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Matcher

newtype EliyahAshevakDogHandler = EliyahAshevakDogHandler EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eliyahAshevakDogHandler :: EnemyCard EliyahAshevakDogHandler
eliyahAshevakDogHandler = enemy EliyahAshevakDogHandler Cards.eliyahAshevakDogHandler (1, Static 3, 4) (1, 1)

instance HasAbilities EliyahAshevakDogHandler where
  getAbilities (EliyahAshevakDogHandler a) =
    extend
      a
      [ skillTestAbility $ restricted a 1 OnSameLocation parleyAction_
      , mkAbility a 2 $ forced $ EnemyDefeated #when Anyone ByAny (be a)
      ]

instance RunMessage EliyahAshevakDogHandler where
  runMessage msg e@(EliyahAshevakDogHandler attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #agility (Fixed 2)
      doStep 2 msg
      pure e
    DoStep 2 (UseThisAbility _iid (isSource attrs -> True) 1) -> do
      n <- perPlayer 2
      when (attrs.token #resource >= n) $ addToVictory attrs
      pure e
    PassedThisSkillTestBy _ (isAbilitySource attrs 1 -> True) n -> do
      placeTokens (attrs.ability 1) attrs #resource n
      pure e
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      initiateEnemyAttack attrs (attrs.ability 1) iid
      pure e
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      eliminatePartner attrs
      pure e
    _ -> EliyahAshevakDogHandler <$> liftRunMessage msg attrs
