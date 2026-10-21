module Arkham.Location.Cards.TracesToNowhere.BonePit (bonePit) where

import Arkham.Ability
import Arkham.Helpers.Cost (getSpendableClueCount)
import Arkham.I18n
import Arkham.Location.CardDefs.TracesToNowhere qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Scenarios.TracesToNowhere.Helpers
import Data.List.NonEmpty qualified as NE

newtype BonePit = BonePit LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bonePit :: LocationCard BonePit
bonePit = location BonePit Cards.bonePit 2 (PerPlayer 1)

instance HasAbilities BonePit where
  getAbilities (BonePit a) =
    extendRevealed
      a
      [ restricted a 1 (exists $ investigatorAt a) $ forced $ PhaseEnds #when #investigation
      , restricted a 2 (Here <> thisExists a (LocationWithCardsUnderneath $ LengthIs $ atLeast 1))
          $ actionAbilityWithCost (GroupClueCost (Static 1) (be a))
      ]

instance RunMessage BonePit where
  runMessage msg l@(BonePit attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      investigators <- select $ investigatorAt attrs
      for_ investigators \iid -> chooseOneM iid $ withI18n do
        countVar 1 $ labeled "takeDamage" $ assignDamage iid (attrs.ability 1) 1
        countVar 1 $ labeled "takeHorror" $ assignHorror iid (attrs.ability 1) 1
      pure l
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      moveOneCardToThePortal attrs
      doStep 2 msg
      pure l
    DoStep n msg'@(UseThisAbility iid (isSource attrs -> True) 2) | n <= 3 -> do
      here <- select (investigatorAt attrs)
      spendable <- getSpendableClueCount here
      remaining <- field LocationCardsUnderneath attrs.id
      when (spendable >= 1 && notNull remaining) do
        chooseOneM iid $ scenarioI18n do
          labeled "bonePit.stop" nothing
          labeled "bonePit.spendAnother" do
            push $ SpendClues 1 here
            moveOneCardToThePortal attrs
            doStep (n + 1) msg'
      pure l
    _ -> BonePit <$> liftRunMessage msg attrs

-- | "choose 1 card at random from beneath this location and place it beneath The Portal"
moveOneCardToThePortal :: ReverseQueue m => LocationAttrs -> m ()
moveOneCardToThePortal attrs =
  field LocationCardsUnderneath attrs.id >>= \case
    [] -> pure ()
    cards -> do
      card <- sample (NE.fromList cards)
      push $ RemoveFromUnderneath (toTarget attrs) [card]
      selectForMaybeM (locationIs Cards.thePortal) \portal ->
        placeUnderneath portal [card]
