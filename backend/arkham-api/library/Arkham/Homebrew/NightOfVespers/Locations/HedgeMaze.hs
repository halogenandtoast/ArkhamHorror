module Arkham.Homebrew.NightOfVespers.Locations.HedgeMaze (hedgeMaze) where

import Arkham.Ability
import Arkham.Homebrew.NightOfVespers.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.NightOfVespers.Scenarios.TheEndOfSeasons.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Log

newtype HedgeMaze = HedgeMaze LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hedgeMaze :: LocationCard HedgeMaze
hedgeMaze = symbolLabel $ location HedgeMaze Cards.hedgeMaze 4 (PerPlayer 2)

-- Printed on the unrevealed side but offered on both, because the window resolves
-- after this location is revealed. It rides the after-move window so that the
-- act's after-enter removal of unrevealed locations resolves first.
instance HasAbilities HedgeMaze where
  getAbilities (HedgeMaze a) =
    extend a
      $ mkAbility a 1 (forced $ Moves #after You AnySource Anywhere (be a))
      : ( guard a.revealed
            *> [skillTestAbility $ restricted a 2 (Here <> NoCluesOnThis) actionAbility]
        )

instance RunMessage HedgeMaze where
  runMessage msg l@(HedgeMaze attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeSetAsideLocationsMatching_
        $ mapOneOf
          cardIs
          [ Cards.grandBallroom
          ]
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 2) iid #willpower (Fixed 3)
      pure l
    PassedThisSkillTest _ (isAbilitySource attrs 2 -> True) -> do
      remember foundAWayThroughTheHedgeMaze
      pure l
    _ -> HedgeMaze <$> liftRunMessage msg attrs
