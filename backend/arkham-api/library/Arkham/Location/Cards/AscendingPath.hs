module Arkham.Location.Cards.AscendingPath (ascendingPath) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Scenarios.WhereDoomAwaits.Helpers

newtype AscendingPath = AscendingPath LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ascendingPath :: LocationCard AscendingPath
ascendingPath =
  locationWith AscendingPath Cards.ascendingPath 3 (Static 0)
    $ revealedConnectedMatchersL
    <>~ ["Altered Path"]

instance HasModifiersFor AscendingPath where
  getModifiersFor (AscendingPath l) = whenUnrevealed l $ modifySelf l [Blocked]

instance HasAbilities AscendingPath where
  getAbilities (AscendingPath attrs) =
    extendRevealed1 attrs
      $ scenarioI18n
      $ withI18nTooltip "ascendingPath.investigate"
      $ playerLimit PerRound
      $ investigateAbility attrs 1 mempty Here

instance RunMessage AscendingPath where
  runMessage msg l@(AscendingPath attrs) = runQueueT case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      investigate sid iid (attrs.ability 1)
      pure l
    Successful (Action.Investigate, _) _ (isAbilitySource attrs 1 -> True) _ _ -> do
      alteredPaths <- getSetAsideCardsMatching "Altered Path"
      for_ (nonEmpty alteredPaths) (sample >=> placeLocation_)
      pure l
    _ -> AscendingPath <$> liftRunMessage msg attrs
