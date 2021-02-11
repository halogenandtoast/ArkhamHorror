module Arkham.Types.Treachery.Cards.AttractingAttention
  ( attractingAttention
  , AttractingAttention(..)
  ) where

import Arkham.Prelude

import Arkham.Types.Card.CardCode
import Arkham.Types.Classes
import Arkham.Types.LocationMatcher
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner
import Arkham.Types.TreacheryId

newtype AttractingAttention = AttractingAttention TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

attractingAttention :: TreacheryId -> a -> AttractingAttention
attractingAttention uuid _ = AttractingAttention $ baseAttrs uuid "TODO"

instance HasModifiersFor env AttractingAttention where
  getModifiersFor = noModifiersFor

instance HasActions env AttractingAttention where
  getActions i window (AttractingAttention attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env AttractingAttention where
  runMessage msg t@(AttractingAttention attrs@TreacheryAttrs {..}) =
    case msg of
      Revelation iid source | isSource attrs source -> do
        lid <- getId iid
        broodOfYogSothoth <- getSetList (CardCode "02255")

        t <$ unshiftMessages
          (chooseOneAtATime
              iid
              [ MoveToward (EnemyTarget eid) (LocationWithId lid)
              | eid <- broodOfYogSothoth
              ]
          : [Discard (toTarget attrs)]
          )
      _ -> AttractingAttention <$> runMessage msg attrs
