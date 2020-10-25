{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Event.Cards.CrypticResearch4 where

import Arkham.Import

import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner

newtype CrypticResearch4 = CrypticResearch4 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

crypticResearch4 :: InvestigatorId -> EventId -> CrypticResearch4
crypticResearch4 iid uuid = CrypticResearch4 $ baseAttrs iid uuid "01043"

instance HasModifiersFor env CrypticResearch4 where
  getModifiersFor _ _ _ = pure []

instance HasActions env CrypticResearch4 where
  getActions i window (CrypticResearch4 attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env CrypticResearch4 where
  runMessage msg (CrypticResearch4 attrs@Attrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId -> do
      locationId <- asks (getId @LocationId iid)
      investigatorIds <- asks $ setToList . getSet locationId
      unshiftMessages
        [ Ask iid $ ChooseOne
          [ TargetLabel (InvestigatorTarget iid') [DrawCards iid' 3 False]
          | iid' <- investigatorIds
          ]
        , Discard (EventTarget eid)
        ]
      CrypticResearch4 <$> runMessage msg (attrs & resolved .~ True)
    _ -> CrypticResearch4 <$> runMessage msg attrs
