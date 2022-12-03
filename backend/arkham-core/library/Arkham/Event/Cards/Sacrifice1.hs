module Arkham.Event.Cards.Sacrifice1
  ( sacrifice1
  , Sacrifice1(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.ClassSymbol
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Target

newtype Sacrifice1 = Sacrifice1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sacrifice1 :: EventCard Sacrifice1
sacrifice1 = event Sacrifice1 Cards.sacrifice1

instance RunMessage Sacrifice1 where
  runMessage msg e@(Sacrifice1 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      targets <-
        selectListMap AssetTarget
        $ AssetWithClass Mystic
        <> DiscardableAsset
        <> assetControlledBy iid
      pushAll
        [ chooseOrRunOne
          iid
          [ TargetLabel target [Discard target] | target <- targets ]
        , chooseAmounts
          iid
          "Number of cards and resources"
          (TotalAmountTarget 3)
          [("Cards", (0, 3)), ("Resources", (0, 3))]
          (toTarget attrs)
        , Discard (toTarget attrs)
        ]
      pure e
    ResolveAmounts iid choices (isTarget attrs -> True) -> do
      let
        choicesMap = mapFromList @(HashMap Text Int) choices
        drawAmount = findWithDefault 0 "Cards" choicesMap
        resourcesAmount = findWithDefault 0 "Resources" choicesMap
      pushAll
        $ [ drawCards iid attrs drawAmount | drawAmount > 0 ]
        <> [ TakeResources iid resourcesAmount False | resourcesAmount > 0 ]
      pure e
    _ -> Sacrifice1 <$> runMessage msg attrs
