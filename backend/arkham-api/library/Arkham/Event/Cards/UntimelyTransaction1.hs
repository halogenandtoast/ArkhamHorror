module Arkham.Event.Cards.UntimelyTransaction1 (untimelyTransaction1, UntimelyTransaction1 (..)) where

import Arkham.Card
import Arkham.Classes.HasQueue (evalQueueT)
import Arkham.Cost.Status
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Game.Helpers (getIsPlayable)
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Query (getPlayer)
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Window (defaultWindows)

newtype UntimelyTransaction1 = UntimelyTransaction1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

untimelyTransaction1 :: EventCard UntimelyTransaction1
untimelyTransaction1 = event UntimelyTransaction1 Cards.untimelyTransaction1

instance RunMessage UntimelyTransaction1 where
  runMessage msg e@(UntimelyTransaction1 attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      items <- select $ InHandOf (InvestigatorWithId iid) <> #item
      chooseOne
        iid
        [ targetLabel
          (toCardId item)
          [RevealCard (toCardId item), HandleTargetChoice iid (toSource attrs) (toTarget $ toCardId item)]
        | item <- items
        ]
      pure e
    HandleTargetChoice iid (isSource attrs -> True) (CardIdTarget cid) -> do
      card <- getCard cid
      otherInvestigators <- select $ affectsOthers $ colocatedWith iid <> not_ (InvestigatorWithId iid)
      canAfford <- flip filterM otherInvestigators $ \other -> getIsPlayable other attrs (UnpaidCost NoAction) (defaultWindows other) card
      unless (null canAfford) do
        for_ canAfford \otherInvestigator -> do
          cardResolutionModifier (toCard attrs) attrs otherInvestigator (AsIfInHand card)
        player <- getPlayer iid
        otherPlayers <- forToSnd canAfford getPlayer
        played <- evalQueueT do
          drawCardsIfCan iid attrs 1
          gainResourcesIfCan iid attrs (printedCardCost card)
        focusCards [card] \unfocus ->
          push
            $ AskMap
            $ mapFromList
            $ (player, ChooseOne [Label "No one pays" [unfocus]])
            : [ ( otherPlayer
                , ChooseOne
                    [ Label "Play Card" $ unfocus
                        : PayCardCost otherInvestigator card (defaultWindows otherInvestigator)
                        : played
                    ]
                )
              | (otherInvestigator, otherPlayer) <- otherPlayers
              ]
      pure e
    _ -> UntimelyTransaction1 <$> liftRunMessage msg attrs
