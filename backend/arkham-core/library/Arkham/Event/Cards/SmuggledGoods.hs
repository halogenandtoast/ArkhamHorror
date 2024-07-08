module Arkham.Event.Cards.SmuggledGoods (smuggledGoods, SmuggledGoods (..)) where

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Projection
import Arkham.Trait
import Arkham.Zone

newtype SmuggledGoods = SmuggledGoods EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

smuggledGoods :: EventCard SmuggledGoods
smuggledGoods = event SmuggledGoods Cards.smuggledGoods

instance RunMessage SmuggledGoods where
  runMessage msg e@(SmuggledGoods attrs) = case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      hasIllicitCardInDiscard <- fieldP InvestigatorDiscard (any (member Illicit . toTraits)) iid
      player <- getPlayer iid
      push
        $ chooseOrRunOne player
        $ Label
          "Search deck"
          [search iid attrs iid [fromTopOfDeck 9] (basic $ withTrait Illicit) (DrawFound iid 1)]
        : [ Label
            "Search discard"
            [search iid attrs attrs [(FromDiscard, PutBack)] (basic $ withTrait Illicit) (DrawFound iid 1)]
          | hasIllicitCardInDiscard
          ]
      pure e
    _ -> SmuggledGoods <$> runMessage msg attrs
