module Arkham.Location.Cards.CuriositieShoppe
  ( curiositieShoppe
  , CuriositieShoppe(..)
  ) where

import Arkham.Prelude

import Arkham.Card.CardType
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Target
import Arkham.Trait

newtype CuriositieShoppe = CuriositieShoppe LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

curiositieShoppe :: LocationCard CuriositieShoppe
curiositieShoppe =
  location CuriositieShoppe Cards.curiositieShoppe 2 (PerPlayer 2)

instance HasModifiersFor CuriositieShoppe where
  getModifiersFor (LocationTarget lid) (CuriositieShoppe a) = do
    isNorthside <- lid <=~> locationIs Cards.northside
    pure $ toModifiers
      a
      [ ConnectedToWhen (LocationWithId lid) (LocationWithId $ toId a)
      | isNorthside
      ]
  getModifiersFor (InvestigatorTarget iid) (CuriositieShoppe attrs) =
    pure $ toModifiers
      attrs
      [ ReduceCostOf (CardWithType AssetType <> CardWithTrait Relic) 2
      | iid `member` locationInvestigators attrs
      ]
  getModifiersFor _ _ = pure []

instance RunMessage CuriositieShoppe where
  runMessage msg (CuriositieShoppe attrs) =
    CuriositieShoppe <$> runMessage msg attrs
