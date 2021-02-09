module Arkham.Types.Treachery.Cards.MysteriousChanting where


import Arkham.Types.Card.EncounterCardMatcher
import Arkham.Types.Trait
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype MysteriousChanting = MysteriousChanting TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mysteriousChanting :: TreacheryId -> a -> MysteriousChanting
mysteriousChanting uuid _ = MysteriousChanting $ baseAttrs uuid "01171"

instance HasModifiersFor env MysteriousChanting where
  getModifiersFor = noModifiersFor

instance HasActions env MysteriousChanting where
  getActions i window (MysteriousChanting attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env MysteriousChanting where
  runMessage msg t@(MysteriousChanting attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      lid <- getId @LocationId iid
      enemies <- map unClosestEnemyId <$> getSetList (lid, [Cultist])
      case enemies of
        [] -> t <$ unshiftMessages
          [ FindAndDrawEncounterCard
            iid
            (EncounterCardMatchByType (EnemyType, Just Cultist))
          , Discard $ toTarget attrs
          ]
        xs -> t <$ unshiftMessages
          [ chooseOne iid [ PlaceDoom (EnemyTarget eid) 2 | eid <- xs ]
          , Discard $ toTarget attrs
          ]
    _ -> MysteriousChanting <$> runMessage msg attrs
