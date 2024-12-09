module Arkham.Location.Cards.AbbeyTowerThePathIsOpen (
  abbeyTowerThePathIsOpen,
  AbbeyTowerThePathIsOpen (..),
) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Log
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message (getChoiceAmount)
import Arkham.ScenarioLogKey

newtype AbbeyTowerThePathIsOpen = AbbeyTowerThePathIsOpen LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

abbeyTowerThePathIsOpen :: LocationCard AbbeyTowerThePathIsOpen
abbeyTowerThePathIsOpen =
  location AbbeyTowerThePathIsOpen Cards.abbeyTowerThePathIsOpen 3 (PerPlayer 2)

instance HasModifiersFor AbbeyTowerThePathIsOpen where
  getModifiersFor (AbbeyTowerThePathIsOpen a) =
    if a.revealed
      then modifySelect a (investigatorAt a <> HandWith AnyCards) [CannotDiscoverCluesAt (be a)]
      else blockedUnless a $ remembered FoundTheTowerKey

instance HasAbilities AbbeyTowerThePathIsOpen where
  getAbilities (AbbeyTowerThePathIsOpen a) =
    extendRevealed1 a $ restricted a 1 (Here <> youExist (HandWith $ HasCard NonWeakness)) actionAbility

instance RunMessage AbbeyTowerThePathIsOpen where
  runMessage msg l@(AbbeyTowerThePathIsOpen attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      maxDiscardAmount <- selectCount $ inHandOf iid <> basic NonWeakness
      chooseAmounts
        iid
        "Discard up to 3 cards from your hand"
        (MaxAmountTarget 3)
        [("Cards", (0, maxDiscardAmount))]
        attrs
      pure l
    ResolveAmounts iid (getChoiceAmount "Cards" -> discardAmount) (isTarget attrs -> True) -> do
      repeated discardAmount $ chooseAndDiscardCard iid (attrs.ability 1)
      pure l
    _ -> AbbeyTowerThePathIsOpen <$> liftRunMessage msg attrs
