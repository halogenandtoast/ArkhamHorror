{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.ArcaneBarrier
  ( ArcaneBarrier(..)
  , arcaneBarrier
  )
where

import Arkham.Import

import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype ArcaneBarrier = ArcaneBarrier Attrs
  deriving newtype (Show, ToJSON, FromJSON)

arcaneBarrier :: TreacheryId -> a -> ArcaneBarrier
arcaneBarrier uuid _ = ArcaneBarrier $ baseAttrs uuid "02102"

instance HasModifiersFor env ArcaneBarrier where
  getModifiersFor = noModifiersFor

instance HasActions env ArcaneBarrier where
  getActions i window (ArcaneBarrier attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env ArcaneBarrier where
  runMessage msg (ArcaneBarrier attrs) = ArcaneBarrier <$> runMessage msg attrs
