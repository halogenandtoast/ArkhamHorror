{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.VaultOfEarthlyDemise where

import Arkham.Import

import Arkham.Types.Game.Helpers
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype VaultOfEarthlyDemise = VaultOfEarthlyDemise Attrs
  deriving newtype (Show, ToJSON, FromJSON)

vaultOfEarthlyDemise :: TreacheryId -> a -> VaultOfEarthlyDemise
vaultOfEarthlyDemise uuid _ = VaultOfEarthlyDemise $ baseAttrs uuid "50032b"

instance HasCount PlayerCount env () => HasModifiersFor env VaultOfEarthlyDemise where
  getModifiersFor _ (EnemyTarget eid) (VaultOfEarthlyDemise attrs)
    | Just eid == treacheryAttachedEnemy attrs = do
      let x = fromJustNote "had to be set" (treacheryResources attrs)
      additionalHealth <- getPlayerCountValue (PerPlayer x)
      pure [HealthModifier additionalHealth, EnemyFight x]
  getModifiersFor _ _ _ = pure []

instance HasActions env VaultOfEarthlyDemise where
  getActions i window (VaultOfEarthlyDemise attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env VaultOfEarthlyDemise where
  runMessage msg (VaultOfEarthlyDemise attrs@Attrs {..}) = case msg of
    AttachTreachery tid _ | tid == treacheryId -> do
      actsRemaining <- unActsRemainingCount <$> getCount ()
      VaultOfEarthlyDemise
        <$> runMessage msg (attrs & resources ?~ actsRemaining)
    Discard (TreacheryTarget tid) | tid == treacheryId ->
      error "this cannot leave play"
    _ -> VaultOfEarthlyDemise <$> runMessage msg attrs
