module Arkham.Location.Cards.Southside_294 (southside_294) where

import Arkham.Ability
import Arkham.Card
import Arkham.GameValue
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.InTheClutchesOfChaos.Helpers
import Arkham.Trait (Trait (Power))

newtype Southside_294 = Southside_294 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

southside_294 :: LocationCard Southside_294
southside_294 = location Southside_294 Cards.southside_294 1 (Static 0)

instance HasAbilities Southside_294 where
  getAbilities (Southside_294 attrs) =
    extendRevealed1 attrs
      $ fastAbility attrs 1 Free (withBreaches attrs $ Here <> EncounterDeckIsNotEmpty)

instance RunMessage Southside_294 where
  runMessage msg l@(Southside_294 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      discardTopOfEncounterDeckAndHandle iid (attrs.ability 1) 3 attrs
      pure l
    DiscardedTopOfEncounterDeck iid cards (isSource attrs -> True) (isTarget attrs -> True) -> do
      let powerTreacheries = filterCards (CardWithTrait Power) cards
      act <- selectJust AnyAct
      removeBreaches attrs 1
      placeBreaches act 1

      focusCards cards do
        chooseOneM iid do
          when (null powerTreacheries) $ withI18n $ labeled' "continue" nothing
          targets powerTreacheries (drawCard iid)
      pure l
    _ -> Southside_294 <$> liftRunMessage msg attrs
