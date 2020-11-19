{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.ObscuringFog where

import Arkham.Import

import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype ObscuringFog = ObscuringFog Attrs
  deriving newtype (Show, ToJSON, FromJSON)

obscuringFog :: TreacheryId -> a -> ObscuringFog
obscuringFog uuid _ = ObscuringFog $ baseAttrs uuid "01168"

instance HasModifiersFor env ObscuringFog where
  getModifiersFor _ (LocationTarget lid) (ObscuringFog attrs) =
    pure [ ShroudModifier 2 | lid `elem` treacheryAttachedLocation attrs ]
  getModifiersFor _ _ _ = pure []

instance HasActions env ObscuringFog where
  getActions i window (ObscuringFog attrs) = getActions i window attrs

instance (TreacheryRunner env) => RunMessage env ObscuringFog where
  runMessage msg t@(ObscuringFog attrs@Attrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      currentLocationId <- asks (getId iid)
      obscuringFogCount <- unTreacheryCount
        <$> getCount (currentLocationId, treacheryCardCode)
      if obscuringFogCount > 0
        then pure t -- Revelation did not run
        else do
          unshiftMessage
            $ AttachTreachery treacheryId (LocationTarget currentLocationId)
          ObscuringFog
            <$> runMessage msg (attrs & attachedLocation ?~ currentLocationId)
    SuccessfulInvestigation _ lid | Just lid == treacheryAttachedLocation ->
      t <$ unshiftMessage (Discard (TreacheryTarget treacheryId))
    _ -> ObscuringFog <$> runMessage msg attrs
