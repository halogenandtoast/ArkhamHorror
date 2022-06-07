module Arkham.Treachery.Cards.MysteriousChanting where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Card
import Arkham.Classes
import Arkham.Matcher
import Arkham.Message
import Arkham.Target
import Arkham.Trait
import Arkham.Treachery.Runner

newtype MysteriousChanting = MysteriousChanting TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mysteriousChanting :: TreacheryCard MysteriousChanting
mysteriousChanting = treachery MysteriousChanting Cards.mysteriousChanting

instance RunMessage MysteriousChanting where
  runMessage msg t@(MysteriousChanting attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      enemies <- selectList $ NearestEnemy $ EnemyWithTrait Cultist
      case enemies of
        [] -> t <$ push
          (FindAndDrawEncounterCard
            iid
            (CardWithType EnemyType <> CardWithTrait Cultist)
          )
        xs ->
          t <$ pushAll
            [chooseOne iid [ PlaceDoom (EnemyTarget eid) 2 | eid <- xs ]]
    _ -> MysteriousChanting <$> runMessage msg attrs
