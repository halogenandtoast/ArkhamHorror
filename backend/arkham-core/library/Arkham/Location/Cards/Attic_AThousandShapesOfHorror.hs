{- HLINT ignore "Use camelCase" -}
module Arkham.Location.Cards.Attic_AThousandShapesOfHorror (
  attic_AThousandShapesOfHorror,
  Attic_AThousandShapesOfHorror (..),
)
where

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.ScenarioLogKey (ScenarioLogKey (RecoveredAStrangeKey))

newtype Attic_AThousandShapesOfHorror = Attic_AThousandShapesOfHorror LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

attic_AThousandShapesOfHorror :: LocationCard Attic_AThousandShapesOfHorror
attic_AThousandShapesOfHorror =
  location
    Attic_AThousandShapesOfHorror
    Cards.attic_AThousandShapesOfHorror
    4
    (PerPlayer 1)

instance HasAbilities Attic_AThousandShapesOfHorror where
  getAbilities (Attic_AThousandShapesOfHorror x) =
    extend
      x
      [ restrictedAbility x 1 (Here <> not_ (Remembered RecoveredAStrangeKey))
          $ FastAbility
          $ GroupClueCost (PerPlayer 1) (LocationWithId $ toId x)
      ]

instance RunMessage Attic_AThousandShapesOfHorror where
  runMessage msg l@(Attic_AThousandShapesOfHorror attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      push $ Remember RecoveredAStrangeKey
      pure l
    _ -> Attic_AThousandShapesOfHorror <$> runMessage msg attrs
