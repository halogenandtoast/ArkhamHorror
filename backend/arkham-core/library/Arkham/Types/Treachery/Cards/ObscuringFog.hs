module Arkham.Types.Treachery.Cards.ObscuringFog where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Card.CardCode
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Helpers
import Arkham.Types.Treachery.Runner

newtype ObscuringFog = ObscuringFog TreacheryAttrs
  deriving anyclass (IsTreachery, HasActions)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

obscuringFog :: TreacheryCard ObscuringFog
obscuringFog = treachery ObscuringFog Cards.obscuringFog

instance HasModifiersFor env ObscuringFog where
  getModifiersFor _ (LocationTarget lid) (ObscuringFog attrs) =
    pure
      $ toModifiers attrs [ ShroudModifier 2 | treacheryOnLocation lid attrs ]
  getModifiersFor _ _ _ = pure []

instance TreacheryRunner env => RunMessage env ObscuringFog where
  runMessage msg t@(ObscuringFog attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      currentLocationId <- getId iid
      obscuringFogCount <- unTreacheryCount
        <$> getCount (currentLocationId, toCardCode attrs)
      if obscuringFogCount > 0
        then t <$ push (Discard $ toTarget attrs)
        else do
          t <$ push
            (AttachTreachery treacheryId $ LocationTarget currentLocationId)
    SuccessfulInvestigation _ lid _ | treacheryOnLocation lid attrs ->
      t <$ push (Discard (TreacheryTarget treacheryId))
    _ -> ObscuringFog <$> runMessage msg attrs
