{- HLINT ignore "Use camelCase" -}
module Arkham.Location.Cards.TheDreamEaters.AThousandShapesOfHorror.Attic (
  attic,
  Attic (..),
)
where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.CardDefs.TheDreamEaters.AThousandShapesOfHorror qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.ScenarioLogKey (ScenarioLogKey (RecoveredAStrangeKey))

newtype Attic = Attic LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

attic :: LocationCard Attic
attic =
  location
    Attic
    Cards.attic
    4
    (PerPlayer 1)

instance HasAbilities Attic where
  getAbilities (Attic x) =
    extendRevealed
      x
      [ onlyOnce
          $ restrictedAbility x 1 Here
          $ FastAbility
          $ GroupClueCost (PerPlayer 1) (LocationWithId $ toId x)
      ]

instance RunMessage Attic where
  runMessage msg l@(Attic attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ Remember RecoveredAStrangeKey
      pure l
    _ -> Attic <$> runMessage msg attrs
