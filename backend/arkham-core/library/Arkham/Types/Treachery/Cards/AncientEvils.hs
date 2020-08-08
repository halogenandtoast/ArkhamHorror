{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.AncientEvils where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner
import Arkham.Types.TreacheryId
import ClassyPrelude

newtype AncientEvils = AncientEvils Attrs
  deriving newtype (Show, ToJSON, FromJSON)

ancientEvils :: TreacheryId -> AncientEvils
ancientEvils uuid = AncientEvils $ baseAttrs uuid "01166"

instance (TreacheryRunner env) => RunMessage env AncientEvils where
  runMessage msg t@(AncientEvils attrs@Attrs {..}) = case msg of
    RunTreachery _ tid | tid == treacheryId -> t <$ unshiftMessages
      [ PlaceDoomOnAgenda
      , AdvanceAgendaIfThresholdSatisfied
      , Discard (TreacheryTarget tid)
      ]
    _ -> AncientEvils <$> runMessage msg attrs
