module Arkham.Treachery.Cards.VaultOfEarthlyDemise where

import Arkham.Ability
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype VaultOfEarthlyDemise = VaultOfEarthlyDemise TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

vaultOfEarthlyDemise :: TreacheryCard VaultOfEarthlyDemise
vaultOfEarthlyDemise = treachery VaultOfEarthlyDemise Cards.vaultOfEarthlyDemise

instance HasAbilities VaultOfEarthlyDemise where
  getAbilities (VaultOfEarthlyDemise attrs) =
    [mkAbility attrs 1 $ forced $ EnemySpawns #when Anywhere $ enemyIs Cards.umordhoth]

instance HasModifiersFor VaultOfEarthlyDemise where
  getModifiersFor (VaultOfEarthlyDemise attrs) = case attrs.placement of
    AttachedToEnemy eid -> do
      let x = treacheryResources attrs
      additionalHealth <- getPlayerCountValue (PerPlayer x)
      modified_ attrs eid [HealthModifier additionalHealth, EnemyFight x]
    _ -> pure mempty

instance RunMessage VaultOfEarthlyDemise where
  runMessage msg t@(VaultOfEarthlyDemise attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      actsRemaining <- selectCount $ RemainingActMatcher AnyAct
      push $ PlaceResources (attrs.ability 1) (toTarget attrs) actsRemaining
      pure t
    Discard _ _ (TreacheryTarget tid) | tid == toId attrs -> do
      error "this cannot leave play"
    _ -> VaultOfEarthlyDemise <$> runMessage msg attrs
