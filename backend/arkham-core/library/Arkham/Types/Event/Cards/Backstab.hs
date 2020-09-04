{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Event.Cards.Backstab where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.EventId
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Target
import Lens.Micro

import ClassyPrelude

newtype Backstab = Backstab Attrs
  deriving newtype (Show, ToJSON, FromJSON)

backstab :: InvestigatorId -> EventId -> Backstab
backstab iid uuid = Backstab $ baseAttrs iid uuid "01051"

instance HasActions env investigator Backstab where
  getActions i window (Backstab attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env Backstab where
  runMessage msg (Backstab attrs@Attrs {..}) = case msg of
    InvestigatorPlayEvent iid eid | eid == eventId -> do
      unshiftMessages
        [ ChooseFightEnemy iid SkillAgility [DamageDealt 2] mempty False
        , Discard (EventTarget eid)
        ]
      Backstab <$> runMessage msg (attrs & resolved .~ True)
    _ -> Backstab <$> runMessage msg attrs
