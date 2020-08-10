{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.ObscuringFog where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner
import Arkham.Types.TreacheryId
import ClassyPrelude
import Lens.Micro

newtype ObscuringFog = ObscuringFog Attrs
  deriving newtype (Show, ToJSON, FromJSON)

obscuringFog :: TreacheryId -> ObscuringFog
obscuringFog uuid = ObscuringFog $ baseAttrs uuid "01168"

instance (TreacheryRunner env) => RunMessage env ObscuringFog where
  runMessage msg t@(ObscuringFog attrs@Attrs {..}) = case msg of
    Revelation iid tid | tid == treacheryId -> do
      currentLocationId <- asks (getId iid)
      obscuringFogCount <- unTreacheryCount
        <$> asks (getCount (currentLocationId, treacheryCardCode))
      if obscuringFogCount > 0
        then t <$ unshiftMessage (Discard (TreacheryTarget tid))
        else do
          unshiftMessages
            [ AttachTreacheryToLocation tid currentLocationId
            , AddModifier
              (LocationTarget currentLocationId)
              (ShroudModifier 2 (TreacherySource tid))
            ]
          pure $ ObscuringFog $ attrs & attachedLocation ?~ currentLocationId
    SuccessfulInvestigation _ lid | Just lid == treacheryAttachedLocation ->
      t <$ unshiftMessages
        [ RemoveAllModifiersOnTargetFrom
          (LocationTarget lid)
          (TreacherySource treacheryId)
        , Discard (TreacheryTarget treacheryId)
        ]
    _ -> ObscuringFog <$> runMessage msg attrs
