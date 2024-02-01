module Arkham.Location.Cards.BaseOfTheHill (
  baseOfTheHill,
  BaseOfTheHill (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.GameValue
import Arkham.Helpers.Query
import Arkham.Investigate
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher

newtype BaseOfTheHill = BaseOfTheHill LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

baseOfTheHill :: LocationCard BaseOfTheHill
baseOfTheHill =
  locationWith BaseOfTheHill Cards.baseOfTheHill 3 (Static 0)
    $ revealedConnectedMatchersL
    <>~ [LocationWithTitle "Diverging Path"]

instance HasAbilities BaseOfTheHill where
  getAbilities (BaseOfTheHill attrs) =
    withResignAction
      attrs
      [ withTooltip
        "{action}: _Investigate_. If you succeed, instead of discovering clues, put a random set-aside Diverging Path into play. (Limit once per round.)"
        $ limitedAbility (PlayerLimit PerRound 1)
        $ investigateAbility attrs 1 mempty Here
      | locationRevealed attrs
      ]

instance RunMessage BaseOfTheHill where
  runMessage msg l@(BaseOfTheHill attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      pushM $ mkInvestigate iid (toAbilitySource attrs 1)
      pure l
    Successful (Action.Investigate, _) _ (AbilitySource source 1) _ _ | isSource attrs source -> do
      divergingPaths <- getSetAsideCardsMatching $ CardWithTitle "Diverging Path"
      for_ (nonEmpty divergingPaths) $ \ne -> do
        pushM $ placeLocation_ =<< sample ne
      pure l
    _ -> BaseOfTheHill <$> runMessage msg attrs
