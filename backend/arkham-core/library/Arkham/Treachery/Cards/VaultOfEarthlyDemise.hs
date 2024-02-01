module Arkham.Treachery.Cards.VaultOfEarthlyDemise where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Timing qualified as Timing
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype VaultOfEarthlyDemise = VaultOfEarthlyDemise TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

vaultOfEarthlyDemise :: TreacheryCard VaultOfEarthlyDemise
vaultOfEarthlyDemise =
  treachery VaultOfEarthlyDemise Cards.vaultOfEarthlyDemise

instance HasAbilities VaultOfEarthlyDemise where
  getAbilities (VaultOfEarthlyDemise attrs) =
    [ mkAbility attrs 1
        $ ForcedAbility
        $ EnemySpawns Timing.When Anywhere
        $ enemyIs Cards.umordhoth
    ]

instance HasModifiersFor VaultOfEarthlyDemise where
  getModifiersFor target@(EnemyTarget _) (VaultOfEarthlyDemise attrs)
    | Just target == treacheryAttachedTarget attrs = do
        let x = treacheryResources attrs
        additionalHealth <- getPlayerCountValue (PerPlayer x)
        pure $ toModifiers attrs [HealthModifier additionalHealth, EnemyFight x]
  getModifiersFor _ _ = pure []

instance RunMessage VaultOfEarthlyDemise where
  runMessage msg t@(VaultOfEarthlyDemise attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      actsRemaining <- selectCount $ RemainingActMatcher AnyAct
      t <$ push (PlaceResources (toAbilitySource attrs 1) (toTarget attrs) actsRemaining)
    Discard _ _ (TreacheryTarget tid) | tid == toId attrs -> do
      error "this cannot leave play"
    _ -> VaultOfEarthlyDemise <$> runMessage msg attrs
