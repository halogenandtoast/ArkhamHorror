module Arkham.Enemy.Cards.CosmicEmissaryTheAbyssShattered (cosmicEmissaryTheAbyssShattered) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (InvestigatorKilled)
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher

newtype CosmicEmissaryTheAbyssShattered = CosmicEmissaryTheAbyssShattered EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cosmicEmissaryTheAbyssShattered :: EnemyCard CosmicEmissaryTheAbyssShattered
cosmicEmissaryTheAbyssShattered =
  enemyWith
    CosmicEmissaryTheAbyssShattered
    Cards.cosmicEmissaryTheAbyssShattered
    (4, Static 10, 4)
    (1, 1)
    $ asSelfLocationL
    ?~ "cosmicEmissaryAbyss"

instance HasModifiersFor CosmicEmissaryTheAbyssShattered where
  getModifiersFor (CosmicEmissaryTheAbyssShattered attrs) = do
    n <- perPlayer 2
    modifySelf attrs [HealthModifier n]

instance HasAbilities CosmicEmissaryTheAbyssShattered where
  getAbilities (CosmicEmissaryTheAbyssShattered attrs) =
    extend1 attrs $ forcedAbility attrs 1 $ EnemyDefeated #after Anyone ByAny (be attrs)

instance RunMessage CosmicEmissaryTheAbyssShattered where
  runMessage msg e@(CosmicEmissaryTheAbyssShattered attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      eachInvestigator \iid -> do
        sid <- getRandom
        beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 100)
      pure e
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      kill (attrs.ability 1) iid
      pure e
    _ -> CosmicEmissaryTheAbyssShattered <$> liftRunMessage msg attrs
