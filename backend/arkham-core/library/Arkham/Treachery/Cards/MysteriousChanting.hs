module Arkham.Treachery.Cards.MysteriousChanting where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Matcher
import Arkham.Message
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Cards
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
        [] -> push $ FindAndDrawEncounterCard
          iid
          (CardWithType EnemyType <> CardWithTrait Cultist)
          True
        xs -> pushAll
          [ chooseOne
              iid
              [ targetLabel eid [PlaceDoom (EnemyTarget eid) 2] | eid <- xs ]
          ]
      pure t
    _ -> MysteriousChanting <$> runMessage msg attrs
