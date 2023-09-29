module Arkham.Location.Cards.AscendingPath (
  ascendingPath,
  AscendingPath (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Investigate
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher

newtype AscendingPath = AscendingPath LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ascendingPath :: LocationCard AscendingPath
ascendingPath =
  locationWith AscendingPath Cards.ascendingPath 3 (Static 0)
    $ revealedConnectedMatchersL
    <>~ [LocationWithTitle "Altered Path"]

instance HasModifiersFor AscendingPath where
  getModifiersFor target (AscendingPath l@LocationAttrs {..}) | isTarget l target = do
    pure $ toModifiers l [Blocked | not locationRevealed]
  getModifiersFor _ _ = pure []

instance HasAbilities AscendingPath where
  getAbilities (AscendingPath attrs) =
    withRevealedAbilities attrs
      $ [ withTooltip
            "{action}: _Investigate_. If you succeed, instead of discovering clues, put a random set-aside Altered Path into play. (Limit once per round.)"
            $ limitedAbility (PlayerLimit PerRound 1)
            $ investigateAbility attrs 1 mempty Here
        ]

instance RunMessage AscendingPath where
  runMessage msg l@(AscendingPath attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      pushM $ mkInvestigate iid (toAbilitySource attrs 1)
      pure l
    Successful (Action.Investigate, _) _ (isAbilitySource attrs 1 -> True) _ _ -> do
      alteredPaths <- getSetAsideCardsMatching $ CardWithTitle "Altered Path"
      for_ (nonEmpty alteredPaths) $ \ne -> do
        pushM $ placeLocation_ =<< sample ne
      pure l
    _ -> AscendingPath <$> runMessage msg attrs
