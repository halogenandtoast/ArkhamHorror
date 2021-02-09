module Arkham.Types.Treachery.Cards.RottingRemainsBloodOnTheAltar
  ( rottingRemainsBloodOnTheAltar
  , RottingRemainsBloodOnTheAltar(..)
  )
where


import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Cards.RottingRemains
import Arkham.Types.Treachery.Runner

newtype RottingRemainsBloodOnTheAltar = RottingRemainsBloodOnTheAltar RottingRemains
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor env, HasActions env)

rottingRemainsBloodOnTheAltar
  :: TreacheryId -> a -> RottingRemainsBloodOnTheAltar
rottingRemainsBloodOnTheAltar uuid _ =
  RottingRemainsBloodOnTheAltar . RottingRemains $ baseAttrs uuid "02223"

instance TreacheryRunner env => RunMessage env RottingRemainsBloodOnTheAltar where
  runMessage msg (RottingRemainsBloodOnTheAltar inner) =
    RottingRemainsBloodOnTheAltar <$> runMessage msg inner
