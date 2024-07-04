module Arkham.Asset.Cards.GearedUp (gearedUp, GearedUp (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Helpers.Modifiers qualified as Msg
import Arkham.Matcher
import Arkham.Window (defaultWindows)

newtype GearedUp = GearedUp AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gearedUp :: AssetCard GearedUp
gearedUp = asset GearedUp Cards.gearedUp

instance HasAbilities GearedUp where
  getAbilities (GearedUp attrs) =
    [ playerLimit PerGame $ restrictedAbility attrs 1 ControlsThis $ forced $ TurnBegins #when You
    ]

instance RunMessage GearedUp where
  runMessage msg a@(GearedUp attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ LoseActions iid (attrs.ability 1) 3
      push $ DoStep 1 msg
      pure a
    DoStep _ msg'@(UseThisAbility iid (isSource attrs -> True) 1) -> do
      cards <-
        select
          $ PlayableCardWithCostReduction NoAction 1 (basic $ #asset <> #item)
          <> inHandOf iid
      when (notNull cards) do
        chooseOne iid $ Label "Done Playing Items" []
          : [ targetLabel
              (toCardId card)
              [ Msg.reduceCostOf (attrs.ability 1) card 1
              , PayCardCost iid card (defaultWindows iid)
              , DoStep 1 msg'
              ]
            | card <- cards
            ]
      pure a
    _ -> GearedUp <$> liftRunMessage msg attrs
