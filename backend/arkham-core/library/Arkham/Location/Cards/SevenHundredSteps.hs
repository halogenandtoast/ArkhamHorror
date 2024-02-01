module Arkham.Location.Cards.SevenHundredSteps (
  sevenHundredSteps,
  SevenHundredSteps (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Projection

newtype SevenHundredSteps = SevenHundredSteps LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

sevenHundredSteps :: LocationCard SevenHundredSteps
sevenHundredSteps = location SevenHundredSteps Cards.sevenHundredSteps 2 (PerPlayer 1)

instance HasAbilities SevenHundredSteps where
  getAbilities (SevenHundredSteps attrs) =
    withRevealedAbilities attrs
      $ [ mkAbility attrs 1
            $ ForcedAbility
            $ Leaves #after (You <> HandWith (LengthIs $ atLeast 4))
            $ LocationWithId
            $ toId attrs
        , restrictedAbility attrs 2 Here $ FastAbility Free
        ]

instance RunMessage SevenHundredSteps where
  runMessage msg l@(SevenHundredSteps attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      handSize <- fieldMap InvestigatorHand length iid
      pushWhen (handSize > 3) $ assignHorror iid (toAbilitySource attrs 1) (handSize - 3)
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push $ chooseAndDiscardCard iid (toAbilitySource attrs 2)
      pure l
    _ -> SevenHundredSteps <$> runMessage msg attrs
