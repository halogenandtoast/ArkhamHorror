module Arkham.Types.Treachery.Cards.ChillFromBelow where

import Arkham.Import

import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype ChillFromBelow = ChillFromBelow Attrs
  deriving newtype (Show, ToJSON, FromJSON)

chillFromBelow :: TreacheryId -> a -> ChillFromBelow
chillFromBelow uuid _ = ChillFromBelow $ baseAttrs uuid "50040"

instance HasModifiersFor env ChillFromBelow where
  getModifiersFor = noModifiersFor

instance HasActions env ChillFromBelow where
  getActions i window (ChillFromBelow attrs) = getActions i window attrs

instance (TreacheryRunner env) => RunMessage env ChillFromBelow where
  runMessage msg t@(ChillFromBelow attrs@Attrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> t <$ unshiftMessages
      [ RevelationSkillTest iid source SkillWillpower 3
      , Discard (TreacheryTarget treacheryId)
      ]
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} n
      | isSource attrs source -> do
        handCount <- unCardCount <$> getCount iid
        if handCount < n
          then
            unshiftMessages
            $ replicate handCount (RandomDiscard iid)
            <> [InvestigatorAssignDamage iid source (n - handCount) 0]
          else unshiftMessages $ replicate n (RandomDiscard iid)
        pure t
    _ -> ChillFromBelow <$> runMessage msg attrs
