module Arkham.Types.Treachery.Cards.ObscuringFog where


import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Helpers
import Arkham.Types.Treachery.Runner

newtype ObscuringFog = ObscuringFog TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

obscuringFog :: TreacheryId -> a -> ObscuringFog
obscuringFog uuid _ = ObscuringFog $ baseAttrs uuid "01168"

instance HasModifiersFor env ObscuringFog where
  getModifiersFor _ (LocationTarget lid) (ObscuringFog attrs) =
    pure
      $ toModifiers attrs [ ShroudModifier 2 | treacheryOnLocation lid attrs ]
  getModifiersFor _ _ _ = pure []

instance HasActions env ObscuringFog where
  getActions i window (ObscuringFog attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env ObscuringFog where
  runMessage msg t@(ObscuringFog attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      currentLocationId <- getId iid
      obscuringFogCount <- unTreacheryCount
        <$> getCount (currentLocationId, treacheryCardCode)
      if obscuringFogCount > 0
        then t <$ unshiftMessage (Discard $ toTarget attrs)
        else do
          t <$ unshiftMessage
            (AttachTreachery treacheryId $ LocationTarget currentLocationId)
    SuccessfulInvestigation _ lid _ | treacheryOnLocation lid attrs ->
      t <$ unshiftMessage (Discard (TreacheryTarget treacheryId))
    _ -> ObscuringFog <$> runMessage msg attrs
