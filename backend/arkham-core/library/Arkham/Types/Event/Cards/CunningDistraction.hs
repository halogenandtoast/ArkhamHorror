{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Event.Cards.CunningDistraction where

import Arkham.Import

import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner

newtype CunningDistraction = CunningDistraction Attrs
  deriving newtype (Show, ToJSON, FromJSON)

cunningDistraction :: InvestigatorId -> EventId -> CunningDistraction
cunningDistraction iid uuid = CunningDistraction $ baseAttrs iid uuid "01078"

instance HasModifiersFor env CunningDistraction where
  getModifiersFor = noModifiersFor

instance HasActions env CunningDistraction where
  getActions i window (CunningDistraction attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env CunningDistraction where
  runMessage msg (CunningDistraction attrs@Attrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId -> do
      locationId <- asks (getId @LocationId iid)
      enemyIds <- asks $ setToList . getSet locationId
      unshiftMessages
        $ [ EnemyEvaded iid enemyId | enemyId <- enemyIds ]
        <> [Discard (EventTarget eid)]
      CunningDistraction <$> runMessage msg (attrs & resolved .~ True)
    _ -> CunningDistraction <$> runMessage msg attrs
