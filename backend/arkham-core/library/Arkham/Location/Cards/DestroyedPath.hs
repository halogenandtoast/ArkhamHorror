module Arkham.Location.Cards.DestroyedPath (
  destroyedPath,
  DestroyedPath (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.GameValue
import Arkham.Investigate
import Arkham.Location.Cards qualified as Cards (destroyedPath)
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher

newtype DestroyedPath = DestroyedPath LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

destroyedPath :: LocationCard DestroyedPath
destroyedPath = location DestroyedPath Cards.destroyedPath 3 (Static 0)

instance HasAbilities DestroyedPath where
  getAbilities (DestroyedPath attrs) =
    withRevealedAbilities attrs
      $ [ mkAbility attrs 1 $ ForcedAbility $ RevealLocation #after You $ LocationWithId $ toId attrs
        , withTooltip
            "{action}: _Investigate_. If you succeed, instead of discovering clues, remove 1 doom from Destroyed Path."
            $ investigateAbility attrs 2 mempty Here
        ]

instance RunMessage DestroyedPath where
  runMessage msg l@(DestroyedPath attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      amount <- perPlayer 1
      push $ PlaceDoom (toAbilitySource attrs 1) (toTarget attrs) amount
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      pushM $ mkInvestigate iid (toAbilitySource attrs 2)
      pure l
    Successful (Action.Investigate, _) _ (isAbilitySource attrs 2 -> True) _ _ -> do
      push $ RemoveDoom (toAbilitySource attrs 2) (toTarget attrs) 1
      pure l
    _ -> DestroyedPath <$> runMessage msg attrs
