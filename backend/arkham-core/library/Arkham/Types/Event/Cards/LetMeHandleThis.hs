module Arkham.Types.Event.Cards.LetMeHandleThis
  ( letMeHandleThis
  , LetMeHandleThis(..)
  )
where

import Arkham.Import

import Arkham.Types.Event.Attrs

newtype LetMeHandleThis = LetMeHandleThis Attrs
  deriving newtype (Show, ToJSON, FromJSON)

letMeHandleThis :: InvestigatorId -> EventId -> LetMeHandleThis
letMeHandleThis iid uuid = LetMeHandleThis $ baseAttrs iid uuid "03022"

instance HasModifiersFor env LetMeHandleThis where
  getModifiersFor = noModifiersFor

instance HasActions env LetMeHandleThis where
  getActions iid window (LetMeHandleThis attrs) = getActions iid window attrs

instance HasQueue env => RunMessage env LetMeHandleThis where
  runMessage msg e@(LetMeHandleThis attrs@Attrs {..}) = case msg of
    InvestigatorPlayEvent iid eid (Just (TreacheryTarget tid))
      | eid == eventId -> do
        withQueue $ \queue ->
          let
            messages = flip map queue $ \case
              Revelation _ (TreacherySource tid') | tid == tid' ->
                Revelation iid (TreacherySource tid')
              AfterRevelation _ tid' | tid == tid' -> AfterRevelation iid tid'
              Surge _ (TreacherySource tid') | tid == tid' ->
                Surge iid (TreacherySource tid')
              other -> other
          in (messages, ())
        e <$ unshiftMessages
          [ CreateEffect
            eventCardCode
            Nothing
            (toSource attrs)
            (InvestigatorTarget iid)
          , Discard (toTarget attrs)
          ]
    _ -> LetMeHandleThis <$> runMessage msg attrs
