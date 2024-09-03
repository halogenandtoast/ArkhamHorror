module Arkham.Location.Cards.SevenHundredSteps (sevenHundredSteps, SevenHundredSteps (..)) where

import Arkham.GameValue
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Projection

newtype SevenHundredSteps = SevenHundredSteps LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sevenHundredSteps :: LocationCard SevenHundredSteps
sevenHundredSteps = location SevenHundredSteps Cards.sevenHundredSteps 2 (PerPlayer 1)

instance HasAbilities SevenHundredSteps where
  getAbilities (SevenHundredSteps attrs) =
    extendRevealed
      attrs
      [ mkAbility attrs 1 $ forced $ Leaves #when (You <> HandWith (LengthIs $ atLeast 4)) (be attrs)
      , restrictedAbility attrs 2 Here $ FastAbility Free
      ]

instance RunMessage SevenHundredSteps where
  runMessage msg l@(SevenHundredSteps attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      handSize <- fieldMap InvestigatorHand length iid
      pushWhen (handSize > 3) $ assignHorror iid (attrs.ability 1) (handSize - 3)
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push $ chooseAndDiscardCard iid (attrs.ability 2)
      pure l
    _ -> SevenHundredSteps <$> runMessage msg attrs
