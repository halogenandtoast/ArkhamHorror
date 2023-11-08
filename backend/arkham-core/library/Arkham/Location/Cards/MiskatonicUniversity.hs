module Arkham.Location.Cards.MiskatonicUniversity (
  MiskatonicUniversity (..),
  miskatonicUniversity,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (miskatonicUniversity)
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Trait

newtype MiskatonicUniversity = MiskatonicUniversity LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

miskatonicUniversity :: LocationCard MiskatonicUniversity
miskatonicUniversity =
  location MiskatonicUniversity Cards.miskatonicUniversity 4 (PerPlayer 2)

instance HasAbilities MiskatonicUniversity where
  getAbilities (MiskatonicUniversity x) =
    withRevealedAbilities x
      $ [restrictedAbility x 1 Here $ ActionAbility [] $ ActionCost 1]

instance RunMessage MiskatonicUniversity where
  runMessage msg l@(MiskatonicUniversity attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push
        $ search
          iid
          (toAbilitySource attrs 1)
          (toTarget iid)
          [fromTopOfDeck 6]
          (CardWithOneOf $ map CardWithTrait [Tome, Spell])
          (DrawFound iid 1)
      pure l
    _ -> MiskatonicUniversity <$> runMessage msg attrs
