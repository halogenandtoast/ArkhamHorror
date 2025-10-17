module Arkham.Location.Cards.TheCabildo (theCabildo) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Helpers.Cost
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype TheCabildo = TheCabildo LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCabildo :: LocationCard TheCabildo
theCabildo = setLabel "e" $ location TheCabildo Cards.theCabildo 4 (Static 2)

instance HasAbilities TheCabildo where
  getAbilities (TheCabildo a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> thisExists a LocationWithConcealedCard)
      $ FastAbility Free

instance RunMessage TheCabildo where
  runMessage msg l@(TheCabildo attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      concealedCards <- map toId <$> getConcealedAtAll attrs.id
      chooseOneM iid $ targets concealedCards $ revealConcealed iid (attrs.ability 1)
      drawEncounterCardEdit iid (attrs.ability 1) (setTarget attrs)
      pure l
    DrewCards iid drewCards | maybe False (isTarget attrs) drewCards.target -> do
      case drewCards.cards of
        (card : rest) -> do
          n <- getSpendableClueCount [iid]
          if n >= 2
            then focusCards drewCards.cards do
              chooseOneM iid $ withI18n do
                countVar 2 $ labeled' "spendClues" do
                  spendClues iid 2
                  cancelCardEffects (attrs.ability 1) card
                  drawCard iid card
                labeled' "doNotSpendClues" $ drawCard iid card
            else drawCard iid card
          for_ rest (drawCard iid)
        _ -> error "expected exactly one card to be drawn"
      pure l
    _ -> TheCabildo <$> liftRunMessage msg attrs
