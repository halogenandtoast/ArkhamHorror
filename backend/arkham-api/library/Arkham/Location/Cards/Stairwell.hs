module Arkham.Location.Cards.Stairwell (stairwell, Stairwell (..)) where

import Arkham.GameValue
import Arkham.Helpers.Window
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Movement
import Arkham.Prelude
import Arkham.Scenarios.WakingNightmare.Helpers
import Arkham.Trait (Trait (Basement))

newtype Stairwell = Stairwell LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stairwell :: LocationCard Stairwell
stairwell = location Stairwell Cards.stairwell 3 (PerPlayer 1)

instance HasAbilities Stairwell where
  getAbilities (Stairwell attrs) =
    withRevealedAbilities
      attrs
      [ skillTestAbility
          $ mkAbility attrs 1
          $ freeReaction
          $ moves #after You (not_ $ LocationWithTrait Basement) attrs
      , mkAbility attrs 2 $ ForcedAbility $ BecomesInfested #after $ LocationWithId $ toId attrs
      ]

instance RunMessage Stairwell where
  runMessage msg l@(Stairwell attrs) =
    case msg of
      UseThisAbility iid (isSource attrs -> True) 1 -> do
        basementLocations <- select $ LocationWithTrait Basement
        player <- getPlayer iid
        sid <- getRandom
        push
          $ chooseOne
            player
            [ targetLabel
              basementLocation
              [ toMessage $ move (attrs.ability 1) iid basementLocation
              , beginSkillTest sid iid (attrs.ability 1) iid #agility (Fixed 4)
              ]
            | basementLocation <- basementLocations
            ]
        pure l
      FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
        push $ assignDamage iid (attrs.ability 1) 2
        pure l
      UseThisAbility _iid (isSource attrs -> True) 2 -> do
        pushM makeInfestationTest
        pure l
      _ -> Stairwell <$> runMessage msg attrs
