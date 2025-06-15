module Arkham.Location.Cards.TMGLibrary (tmgLibrary) where

import Arkham.Prelude
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import
import Arkham.Matcher

newtype TMGLibrary = TMGLibrary LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | 'Library' from The Midwinter Gala (#71013).
tmgLibrary :: LocationCard TMGLibrary
tmgLibrary =
  location
    TMGLibrary
    Cards.tmgLibrary
    4
    (PerPlayer 3)
    Hourglass
    [Circle, T, Heart]
    & revealedBy False

instance HasModifiersFor TMGLibrary where
  getModifiersFor target (TMGLibrary attrs)
    | isTarget attrs target = pure $ toModifiers attrs [SkillModifier #any 1]
  getModifiersFor _ _ = pure []

instance HasAbilities TMGLibrary where
  getAbilities (TMGLibrary attrs) =
    withBaseAbilities attrs
      [ limitedAbility (GroupLimit PerGame 1)
          $ restrictedAbility attrs 1 Here
          $ FastAbility Free
      ]

instance RunMessage TMGLibrary where
  runMessage msg l@(TMGLibrary attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      -- TODO: Implement Jewel interaction ability
      pure l
    _ -> TMGLibrary <$> runMessage msg attrs
