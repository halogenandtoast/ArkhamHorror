{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.AncientEvils where

import Arkham.Import

import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype AncientEvils = AncientEvils Attrs
  deriving newtype (Show, ToJSON, FromJSON)

ancientEvils :: TreacheryId -> a -> AncientEvils
ancientEvils uuid _ = AncientEvils $ baseAttrs uuid "01166"

instance HasModifiersFor env AncientEvils where
  getModifiersFor = noModifiersFor

instance HasActions env AncientEvils where
  getActions i window (AncientEvils attrs) = getActions i window attrs

instance (TreacheryRunner env) => RunMessage env AncientEvils where
  runMessage msg (AncientEvils attrs@Attrs {..}) = case msg of
    Revelation _ source | isSource attrs source -> do
      unshiftMessages
        [ PlaceDoomOnAgenda
        , AdvanceAgendaIfThresholdSatisfied
        , Discard (TreacheryTarget treacheryId)
        ]
      AncientEvils <$> runMessage msg (attrs & resolved .~ True)
    _ -> AncientEvils <$> runMessage msg attrs
