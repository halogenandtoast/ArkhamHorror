module Arkham.Types.Treachery.Cards.VaultOfEarthlyDemise where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype VaultOfEarthlyDemise = VaultOfEarthlyDemise TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

vaultOfEarthlyDemise :: TreacheryCard VaultOfEarthlyDemise
vaultOfEarthlyDemise =
  treachery VaultOfEarthlyDemise Cards.vaultOfEarthlyDemise

instance HasAbilities env VaultOfEarthlyDemise where
  getAbilities _ _ (VaultOfEarthlyDemise attrs) = pure
    [ mkAbility attrs 1
      $ ForcedAbility
      $ EnemySpawns Timing.When Anywhere
      $ enemyIs Cards.umordhoth
    ]

instance HasCount PlayerCount env () => HasModifiersFor env VaultOfEarthlyDemise where
  getModifiersFor _ target@(EnemyTarget _) (VaultOfEarthlyDemise attrs)
    | Just target == treacheryAttachedTarget attrs = do
      let x = fromJustNote "had to be set" (treacheryResources attrs)
      additionalHealth <- getPlayerCountValue (PerPlayer x)
      pure $ toModifiers attrs [HealthModifier additionalHealth, EnemyFight x]
  getModifiersFor _ _ _ = pure []

instance TreacheryRunner env => RunMessage env VaultOfEarthlyDemise where
  runMessage msg t@(VaultOfEarthlyDemise attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      actsRemaining <- unActsRemainingCount <$> getCount ()
      t <$ push (PlaceResources (toTarget attrs) actsRemaining)
    Discard (TreacheryTarget tid) | tid == toId attrs ->
      error "this cannot leave play"
    _ -> VaultOfEarthlyDemise <$> runMessage msg attrs
