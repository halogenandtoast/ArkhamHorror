module Arkham.Location.Cards.Easttown where

import Arkham.Prelude

import Arkham.Card.CardType
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards ( easttown )
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Target
import Arkham.Trait

newtype Easttown = Easttown LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

easttown :: LocationCard Easttown
easttown = location Easttown Cards.easttown 2 (PerPlayer 1)

instance HasModifiersFor Easttown where
  getModifiersFor (InvestigatorTarget iid) (Easttown attrs) =
    pure $ toModifiers
      attrs
      [ ReduceCostOf (CardWithType AssetType <> CardWithTrait Ally) 2
      | iid `member` locationInvestigators attrs
      ]
  getModifiersFor _ _ = pure []

instance RunMessage Easttown where
  runMessage msg (Easttown attrs) = Easttown <$> runMessage msg attrs
