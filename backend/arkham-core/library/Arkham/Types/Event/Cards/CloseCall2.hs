{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Event.Cards.CloseCall2 where

import Arkham.Import

import Arkham.Types.Event.Attrs

newtype CloseCall2 = CloseCall2 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

closeCall2 :: InvestigatorId -> EventId -> CloseCall2
closeCall2 iid uuid = CloseCall2 $ baseAttrs iid uuid "01083"

instance HasModifiersFor env CloseCall2 where
  getModifiersFor = noModifiersFor

instance HasActions env CloseCall2 where
  getActions i window (CloseCall2 attrs) = getActions i window attrs

instance HasQueue env => RunMessage env CloseCall2 where
  runMessage msg e@(CloseCall2 attrs@Attrs {..}) = case msg of
    InvestigatorPlayEvent _iid eid (Just (EnemyTarget enemyId))
      | eid == eventId -> e <$ unshiftMessages
        [ ShuffleBackIntoEncounterDeck (EnemyTarget enemyId)
        , Discard (toTarget attrs)
        ]
    _ -> CloseCall2 <$> runMessage msg attrs
