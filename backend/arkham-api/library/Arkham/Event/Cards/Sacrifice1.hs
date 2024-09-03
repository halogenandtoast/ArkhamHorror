module Arkham.Event.Cards.Sacrifice1 (
  sacrifice1,
  Sacrifice1 (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher

newtype Sacrifice1 = Sacrifice1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sacrifice1 :: EventCard Sacrifice1
sacrifice1 = event Sacrifice1 Cards.sacrifice1

instance RunMessage Sacrifice1 where
  runMessage msg e@(Sacrifice1 attrs) = case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      targets <- selectTargets $ #mystic <> DiscardableAsset <> assetControlledBy iid
      player <- getPlayer iid
      chooseMsg <-
        chooseAmounts
          player
          "Number of cards and resources"
          (TotalAmountTarget 3)
          [("Cards", (0, 3)), ("Resources", (0, 3))]
          attrs

      pushAll
        [ chooseOrRunOne player $ targetLabels targets $ only . toDiscardBy iid attrs
        , chooseMsg
        ]
      pure e
    ResolveAmounts iid choices (isTarget attrs -> True) -> do
      let drawAmount = getChoiceAmount "Cards" choices
      let resourcesAmount = getChoiceAmount "Resources" choices
      let drawing = drawCards iid attrs drawAmount
      pushAll
        $ [drawing | drawAmount > 0]
        <> [TakeResources iid resourcesAmount (toSource attrs) False | resourcesAmount > 0]
      pure e
    _ -> Sacrifice1 <$> runMessage msg attrs
