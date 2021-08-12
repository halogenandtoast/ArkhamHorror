module Arkham.Types.Treachery.Cards.MysteriousChanting where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Trait
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype MysteriousChanting = MysteriousChanting TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasActions)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mysteriousChanting :: TreacheryCard MysteriousChanting
mysteriousChanting = treachery MysteriousChanting Cards.mysteriousChanting

instance TreacheryRunner env => RunMessage env MysteriousChanting where
  runMessage msg t@(MysteriousChanting attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      lid <- getId @LocationId iid
      enemies <- map unClosestEnemyId <$> getSetList (lid, [Cultist])
      case enemies of
        [] -> t <$ pushAll
          [ FindAndDrawEncounterCard
            iid
            (CardWithType EnemyType <> CardWithTrait Cultist)
          , Discard $ toTarget attrs
          ]
        xs -> t <$ pushAll
          [ chooseOne iid [ PlaceDoom (EnemyTarget eid) 2 | eid <- xs ]
          , Discard $ toTarget attrs
          ]
    _ -> MysteriousChanting <$> runMessage msg attrs
