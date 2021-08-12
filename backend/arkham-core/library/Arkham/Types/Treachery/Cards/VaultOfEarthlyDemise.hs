module Arkham.Types.Treachery.Cards.VaultOfEarthlyDemise where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype VaultOfEarthlyDemise = VaultOfEarthlyDemise TreacheryAttrs
  deriving anyclass (IsTreachery, HasActions)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

vaultOfEarthlyDemise :: TreacheryCard VaultOfEarthlyDemise
vaultOfEarthlyDemise =
  treachery VaultOfEarthlyDemise Cards.vaultOfEarthlyDemise

instance HasCount PlayerCount env () => HasModifiersFor env VaultOfEarthlyDemise where
  getModifiersFor _ target@(EnemyTarget _) (VaultOfEarthlyDemise attrs)
    | Just target == treacheryAttachedTarget attrs = do
      let x = fromJustNote "had to be set" (treacheryResources attrs)
      additionalHealth <- getPlayerCountValue (PerPlayer x)
      pure $ toModifiers attrs [HealthModifier additionalHealth, EnemyFight x]
  getModifiersFor _ _ _ = pure []

instance TreacheryRunner env => RunMessage env VaultOfEarthlyDemise where
  runMessage msg (VaultOfEarthlyDemise attrs@TreacheryAttrs {..}) = case msg of
    AttachTreachery tid _ | tid == treacheryId -> do
      actsRemaining <- unActsRemainingCount <$> getCount ()
      VaultOfEarthlyDemise
        <$> runMessage msg (attrs & resourcesL ?~ actsRemaining)
    Discard (TreacheryTarget tid) | tid == treacheryId ->
      error "this cannot leave play"
    _ -> VaultOfEarthlyDemise <$> runMessage msg attrs
