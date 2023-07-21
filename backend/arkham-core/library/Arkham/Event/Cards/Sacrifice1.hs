module Arkham.Event.Cards.Sacrifice1 (
  sacrifice1,
  Sacrifice1 (..),
) where

import Arkham.Prelude

import Arkham.ClassSymbol
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher
import Arkham.Message

newtype Sacrifice1 = Sacrifice1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sacrifice1 :: EventCard Sacrifice1
sacrifice1 = event Sacrifice1 Cards.sacrifice1

instance RunMessage Sacrifice1 where
  runMessage msg e@(Sacrifice1 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      targets <-
        selectListMap AssetTarget $
          AssetWithClass Mystic
            <> DiscardableAsset
            <> assetControlledBy iid
      pushAll
        [ chooseOrRunOne
            iid
            [TargetLabel target [Discard (toSource attrs) target] | target <- targets]
        , chooseAmounts
            iid
            "Number of cards and resources"
            (TotalAmountTarget 3)
            [("Cards", (0, 3)), ("Resources", (0, 3))]
            (toTarget attrs)
        ]
      pure e
    ResolveAmounts iid choices (isTarget attrs -> True) -> do
      let
        drawAmount = getChoiceAmount "Cards" choices
        resourcesAmount = getChoiceAmount "Resources" choices
      drawing <- drawCards iid attrs drawAmount
      pushAll $
        [drawing | drawAmount > 0]
          <> [TakeResources iid resourcesAmount (toSource attrs) False | resourcesAmount > 0]
      pure e
    _ -> Sacrifice1 <$> runMessage msg attrs
