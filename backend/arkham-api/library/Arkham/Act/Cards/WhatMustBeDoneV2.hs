module Arkham.Act.Cards.WhatMustBeDoneV2 (whatMustBeDoneV2) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Campaigns.TheCircleUndone.Key
import Arkham.Campaigns.TheCircleUndone.Memento
import Arkham.Card
import Arkham.Draw.Types
import Arkham.Helpers.Log (inRecordSet)
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Message.Lifted.Choose
import Arkham.Movement
import Arkham.Scenario.Deck
import Arkham.Scenarios.BeforeTheBlackThrone.Helpers
import Data.List qualified as List

newtype WhatMustBeDoneV2 = WhatMustBeDoneV2 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whatMustBeDoneV2 :: ActCard WhatMustBeDoneV2
whatMustBeDoneV2 = act (3, A) WhatMustBeDoneV2 Cards.whatMustBeDoneV2 Nothing

instance HasAbilities WhatMustBeDoneV2 where
  getAbilities (WhatMustBeDoneV2 attrs) =
    extend
      attrs
      [ mkAbility attrs 1 $ actionAbilityWithCost ClueCostX
      , restricted
          attrs
          2
          (exists $ LeadInvestigator <> at_ ("The Black Throne" <> LocationWithoutClues))
          $ Objective
          $ forced AnyWindow
      ]

getClueCount :: Payment -> Int
getClueCount (CluePayment _ n) = n
getClueCount (Payments ps) = sum $ map getClueCount ps
getClueCount _ = 0

instance RunMessage WhatMustBeDoneV2 where
  runMessage msg a@(WhatMustBeDoneV2 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ (getClueCount -> x) -> do
      push $ DrawCards iid $ targetCardDraw attrs CosmosDeck x
      pure a
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    DrewCards iid drewCards | maybe False (isTarget attrs) drewCards.target -> do
      let cards = drewCards.cards

      focusCards (map showRevealed drewCards.cards) do
        chooseOrRunOneM iid do
          targets drewCards.cards \card -> do
            unfocusCards
            shuffleCardsIntoDeck CosmosDeck (List.delete card cards)
            lid <- placeLocation card
            revealBy iid lid
            movemsg <- move (attrs.ability 1) iid lid
            push $ RunCosmos iid lid [Move movemsg]
      pure a
    AdvanceAct aid _ _ | aid == toId a && onSide B attrs -> do
      youAcceptedYourFate <- getHasRecord YouHaveAcceptedYourFate
      haveMesmerizingFlute <- MesmerizingFlute `inRecordSet` MementosDiscovered
      haveRitualComponents <- RitualComponents `inRecordSet` MementosDiscovered

      youRejectedYourFate <- getHasRecord YouHaveRejectedYourFate
      haveScrapOfTornShadow <- ScrapOfTornShadow `inRecordSet` MementosDiscovered
      haveWispOfSpectralMist <- WispOfSpectralMist `inRecordSet` MementosDiscovered

      haveStrangeIncantation <- StrangeIncantation `inRecordSet` MementosDiscovered
      haveBloodyTreeCarvings <- BloodyTreeCarvings `inRecordSet` MementosDiscovered

      leadChooseOrRunOneM $ scenarioI18n do
        when (count id [youAcceptedYourFate, haveMesmerizingFlute, haveRitualComponents] >= 2) do
          labeled' "whatMustBeDone.R2" $ push R2
        when (count id [youRejectedYourFate, haveScrapOfTornShadow, haveWispOfSpectralMist] >= 2) do
          labeled' "whatMustBeDone.R3" $ push R3
        when (haveStrangeIncantation && haveBloodyTreeCarvings) do
          labeled' "whatMustBeDone.R6" $ push R6
        labeled' "whatMustBeDone.R4" $ push R4
      pure a
    _ -> WhatMustBeDoneV2 <$> liftRunMessage msg attrs
