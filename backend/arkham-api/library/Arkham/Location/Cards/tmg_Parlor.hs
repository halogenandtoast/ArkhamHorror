module Arkham.Location.Cards.TMGParlor (tmgParlor) where

import Arkham.Prelude
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import
import Arkham.Matcher

newtype TMGParlor = TMGParlor LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | 'Parlor' from The Midwinter Gala (#71014).
tmgParlor :: LocationCard TMGParlor
tmgParlor =
  location
    TMGParlor
    Cards.tmgParlor
    6
    (PerPlayer 3)
    Circle
    [T, Hourglass, Heart]
    & revealedBy False

instance HasModifiersFor TMGParlor where
  getModifiersFor target (TMGParlor attrs)
    | isTarget attrs target = do
        guests <- selectCount $ assetAt (toId attrs) <> AssetWithTrait Guest
        pure $ toModifiers attrs [ShroudModifier (-min guests 5)]
  getModifiersFor _ _ = pure []

instance HasAbilities TMGParlor where
  getAbilities (TMGParlor attrs) =
    withBaseAbilities attrs
      [ limitedAbility (GroupLimit PerGame 1)
          $ restrictedAbility attrs 1 Here
          $ FastAbility Free
      ]

instance RunMessage TMGParlor where
  runMessage msg l@(TMGParlor attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      -- TODO: Implement Jewel exhaust ability
      pure l
    _ -> TMGParlor <$> runMessage msg attrs
