module Arkham.Types.Event.Cards.CloseCall2 where


import Arkham.Types.Event.Attrs
import Arkham.Types.Trait

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
        [ InitiatePlayCard iid (getCardId attrs) (Just $ EnemyTarget eid) False
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
