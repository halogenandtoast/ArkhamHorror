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

obscuringFog :: TreacheryId -> a -> ObscuringFog
obscuringFog uuid _ = ObscuringFog $ baseAttrs uuid "01168"

instance HasActions env ObscuringFog where
  getActions i window (ObscuringFog attrs) = getActions i window attrs

instance (TreacheryRunner env) => RunMessage env ObscuringFog where
  runMessage msg t@(ObscuringFog attrs@Attrs {..}) = case msg of
    Revelation iid tid | tid == treacheryId -> do
      currentLocationId <- asks (getId iid)
      obscuringFogCount <- unTreacheryCount
        <$> asks (getCount (currentLocationId, treacheryCardCode))
      if obscuringFogCount > 0
        then pure t -- Revelation did not run
        else do
          unshiftMessages
            [ AttachTreachery tid (LocationTarget currentLocationId)
            , AddModifiers
              (LocationTarget currentLocationId)
              (TreacherySource tid)
              [ShroudModifier 2]
            ]
          ObscuringFog
            <$> runMessage msg (attrs & attachedLocation ?~ currentLocationId)
    SuccessfulInvestigation _ lid | Just lid == treacheryAttachedLocation ->
      t <$ unshiftMessages
        [ RemoveAllModifiersOnTargetFrom
          (LocationTarget lid)
          (TreacherySource treacheryId)
        , Discard (TreacheryTarget treacheryId)
        ]
    _ -> ObscuringFog <$> runMessage msg attrs
