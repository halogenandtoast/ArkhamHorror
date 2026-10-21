module Arkham.Location.Cards.TracesToNowhere.ThePortal (thePortal) where

import Arkham.Ability
import Arkham.Helpers.Cost (getSpendableClueCount)
import Arkham.Location.CardDefs.TracesToNowhere qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Projection
import Arkham.Scenarios.TracesToNowhere.Helpers
import Arkham.Trait (Trait (Arkham))

newtype ThePortal = ThePortal LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thePortal :: LocationCard ThePortal
thePortal = location ThePortal Cards.thePortal 4 (PerPlayer 1)

instance HasAbilities ThePortal where
  getAbilities (ThePortal a) =
    extendRevealed
      a
      [ restricted a 1 Here
          $ actionAbilityWithCost
          $ OrCost [DirectHorrorCost (toSource a) You 1, DirectDamageCost (toSource a) You 1]
      , restricted
          a
          2
          (Here <> thisExists a (LocationWithCardsUnderneath $ LengthIs $ atLeast 1))
          actionAbility
      ]

instance RunMessage ThePortal where
  runMessage msg l@(ThePortal attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locations <- select $ RevealedLocation <> LocationWithTrait Arkham
      chooseTargetM iid locations $ moveTo (attrs.ability 1) iid
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      rescueFrom iid (toTarget attrs) =<< field LocationCardsUnderneath attrs.id
      doStep 1 msg
      pure l
    DoStep 1 (UseThisAbility iid (isSource attrs -> True) 2) -> do
      spendable <- getSpendableClueCount [iid]
      remaining <- field LocationCardsUnderneath attrs.id
      when (spendable >= 2 && notNull remaining) do
        chooseOneM iid $ scenarioI18n do
          labeled "thePortal.noAdditionalRescue" nothing
          labeled "thePortal.rescueAdditional" do
            spendClues iid 2
            rescueFrom iid (toTarget attrs) remaining
      pure l
    _ -> ThePortal <$> liftRunMessage msg attrs
