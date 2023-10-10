module Arkham.Treachery.Cards.MysteriousChanting where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Matcher
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
    Revelation iid (isSource attrs -> True) -> do
      enemies <- selectList $ NearestEnemy $ EnemyWithTrait Cultist
      case enemies of
        [] -> push $ findAndDrawEncounterCard iid $ CardWithType EnemyType <> CardWithTrait Cultist
        xs -> do
          player <- getPlayer iid
          pushAll
            [ chooseOne player
                $ [targetLabel eid [PlaceDoom (toSource attrs) (toTarget eid) 2] | eid <- xs]
            ]
      pure t
    _ -> MysteriousChanting <$> runMessage msg attrs
