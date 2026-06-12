module Arkham.Enemy.Cards.RavenousSpirit (ravenousSpirit) where

import Arkham.Ability
import Arkham.Campaigns.TheCircleUndone.Helpers (runLocationHauntedAbilities)
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Helpers.Query (getLead)
import Arkham.Matcher
import Arkham.Trait (Trait (Spectral))

newtype RavenousSpirit = RavenousSpirit EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ravenousSpirit :: EnemyCard RavenousSpirit
ravenousSpirit = enemy RavenousSpirit Cards.ravenousSpirit (4, Static 4, 4) (1, 1)

instance HasModifiersFor RavenousSpirit where
  getModifiersFor (RavenousSpirit a) = do
    n <- perPlayer 2
    modifySelf a [HealthModifier n]

instance HasAbilities RavenousSpirit where
  getAbilities (RavenousSpirit a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ oneOf [EnemyEnters #after Anywhere (be a), EnemySpawns #after Anywhere (be a)]

instance RunMessage RavenousSpirit where
  runMessage msg e@(RavenousSpirit attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      withLocationOf attrs \lid -> do
        whenM (lid <=~> not_ (LocationWithTrait Spectral)) do
          lead <- getLead
          flipOverBy lead (attrs.ability 1) lid
      doStep 1 msg
      pure e
    DoStep 1 (UseThisAbility _ (isSource attrs -> True) 1) -> do
      withLocationOf attrs \lid -> do
        investigators <- select $ investigatorAt lid
        for_ investigators \iid -> runLocationHauntedAbilities iid lid
      pure e
    _ -> RavenousSpirit <$> liftRunMessage msg attrs
