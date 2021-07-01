module Arkham.Types.Event.Cards.CloseCall2 where

import Arkham.Prelude

import Arkham.EncounterCard
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.Event.Attrs
import Arkham.Types.EventId
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Trait
import Arkham.Types.Window

newtype CloseCall2 = CloseCall2 EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

closeCall2 :: InvestigatorId -> EventId -> CloseCall2
closeCall2 iid uuid = CloseCall2 $ baseAttrs iid uuid "01083"

instance HasModifiersFor env CloseCall2 where
  getModifiersFor = noModifiersFor

instance (HasId CardCode env EnemyId, HasSet Trait env EnemyId) => HasActions env CloseCall2 where
  getActions iid (InHandWindow ownerId (AfterEnemyEvaded You eid)) (CloseCall2 attrs)
    | iid == ownerId
    = do
      traits' <- getSet eid
      cardCode <- getId eid
      pure
        [ InitiatePlayCard iid (attrs ^. cardIdL) (Just $ EnemyTarget eid) False
        | Elite `notMember` traits' && cardCode `elem` keys allEncounterCards
        ]
  getActions i window (CloseCall2 attrs) = getActions i window attrs

instance HasQueue env => RunMessage env CloseCall2 where
  runMessage msg e@(CloseCall2 attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent _iid eid (Just (EnemyTarget enemyId))
      | eid == eventId -> e <$ unshiftMessages
        [ ShuffleBackIntoEncounterDeck (EnemyTarget enemyId)
        , Discard (toTarget attrs)
        ]
    _ -> CloseCall2 <$> runMessage msg attrs
