module Arkham.Types.Treachery.Cards.AncientEvils where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.TreacheryId


import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype AncientEvils = AncientEvils TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ancientEvils :: TreacheryId -> a -> AncientEvils
ancientEvils uuid _ = AncientEvils $ baseAttrs uuid "01166"

instance HasModifiersFor env AncientEvils where
  getModifiersFor = noModifiersFor

instance HasActions env AncientEvils where
  getActions i window (AncientEvils attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env AncientEvils where
  runMessage msg t@(AncientEvils attrs@TreacheryAttrs {..}) = case msg of
    Revelation _ source | isSource attrs source -> do
      t <$ unshiftMessages
        [ PlaceDoomOnAgenda
        , AdvanceAgendaIfThresholdSatisfied
        , Discard (TreacheryTarget treacheryId)
        ]
    _ -> AncientEvils <$> runMessage msg attrs
