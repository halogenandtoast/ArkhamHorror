module Arkham.Location.Cards.SeventySteps (
  seventySteps,
  SeventySteps (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Projection

newtype SeventySteps = SeventySteps LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

seventySteps :: LocationCard SeventySteps
seventySteps = location SeventySteps Cards.seventySteps 1 (PerPlayer 1)

instance HasAbilities SeventySteps where
  getAbilities (SeventySteps attrs) =
    withRevealedAbilities
      attrs
      [ mkAbility attrs 1
          $ forced
          $ Leaves #after (You <> HandWith (LengthIs $ atLeast 6))
          $ be attrs
      , restrictedAbility attrs 2 (Here <> youExist (HandWith (HasCard DiscardableCard)))
          $ FastAbility Free
      ]

instance RunMessage SeventySteps where
  runMessage msg l@(SeventySteps attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      handSize <- fieldMap InvestigatorHand length iid
      pushWhen (handSize > 5) $ assignHorror iid (toAbilitySource attrs 1) (handSize - 5)
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push $ chooseAndDiscardCard iid (toAbilitySource attrs 2)
      pure l
    _ -> SeventySteps <$> runMessage msg attrs
