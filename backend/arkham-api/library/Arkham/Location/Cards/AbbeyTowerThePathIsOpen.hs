module Arkham.Location.Cards.AbbeyTowerThePathIsOpen (abbeyTowerThePathIsOpen) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Log
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Helpers.Modifiers
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
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
    UseThisAbility iid (isSource attrs -> True) 1 -> withI18n do
      maxDiscardAmount <- selectCount $ inHandOf NotForPlay iid <> basic NonWeakness
      countVar 3 $ chooseAmount' iid "discardCardsFromHandUpTo" "$cards" 0 maxDiscardAmount attrs
      pure l
    ResolveAmounts iid (getChoiceAmount "$cards" -> discardAmount) (isTarget attrs -> True) -> do
      repeated discardAmount $ chooseAndDiscardCard iid (attrs.ability 1)
      pure l
    _ -> AbbeyTowerThePathIsOpen <$> liftRunMessage msg attrs
