module Arkham.Location.Cards.Schoolhouse_212
  ( schoolhouse_212
  , Schoolhouse_212(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards (schoolhouse_212)
import Arkham.Card.CardType
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Runner
import Arkham.Location.Helpers
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Target

newtype Schoolhouse_212 = Schoolhouse_212 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

schoolhouse_212 :: LocationCard Schoolhouse_212
schoolhouse_212 = location
  Schoolhouse_212
  Cards.schoolhouse_212
  4
  (Static 1)
  Moon
  [Plus, Squiggle, Circle]

instance HasModifiersFor env Schoolhouse_212 where
  getModifiersFor _ (InvestigatorTarget iid) (Schoolhouse_212 attrs) =
    pure $ toModifiers
      attrs
      [ CannotCommitCards (CardWithType SkillType)
      | iid `member` locationInvestigators attrs
      ]
  getModifiersFor _ _ _ = pure []

instance HasAbilities Schoolhouse_212 where
  getAbilities = withDrawCardUnderneathAction

instance LocationRunner env => RunMessage Schoolhouse_212 where
  runMessage msg (Schoolhouse_212 attrs) =
    Schoolhouse_212 <$> runMessage msg attrs
