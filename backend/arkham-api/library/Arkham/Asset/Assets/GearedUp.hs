module Arkham.Asset.Assets.GearedUp (gearedUp) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Helpers.Modifiers (withModifiers)
import Arkham.Helpers.Playable (getPlayableCards)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Taboo
import Arkham.Window (defaultWindows)

newtype GearedUp = GearedUp AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gearedUp :: AssetCard GearedUp
gearedUp = asset GearedUp Cards.gearedUp

instance HasAbilities GearedUp where
  getAbilities (GearedUp attrs) =
    [playerLimit PerGame $ restricted attrs 1 ControlsThis $ forced $ TurnBegins #when You]

instance RunMessage GearedUp where
  runMessage msg a@(GearedUp attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      loseActions iid (attrs.ability 1) 3
      if tabooed TabooList21 attrs
        then doStep 5 msg
        else do_ msg
      pure a

    Do msg'@(UseThisAbility iid (isSource attrs -> True) 1) -> do
      cards <- withModifiers iid (toModifiers attrs [ReduceCostOf AnyCard 1]) $ do
        filter (`cardMatch` card_ (#asset <> #item))
          <$> getPlayableCards (attrs.ability 1) iid (UnpaidCost NoAction) (defaultWindows iid)
      when (notNull cards) do
        chooseOneM iid do
          labeled "Done Playing Items" nothing
          targets cards \card -> do
            reduceCostOf (attrs.ability 1) card 1
            push $ PayCardCost iid card (defaultWindows iid)
            do_ msg'
      pure a
    DoStep n msg'@(UseThisAbility iid (isSource attrs -> True) 1) | n > 0 -> do
      cards <- withModifiers iid (toModifiers attrs [ReduceCostOf AnyCard 1]) $ do
        filter (`cardMatch` card_ (#asset <> #item))
          <$> getPlayableCards (attrs.ability 1) iid (UnpaidCost NoAction) (defaultWindows iid)
      when (notNull cards) do
        chooseOneM iid do
          labeled "Done Playing Items" nothing
          targets cards \card -> do
            reduceCostOf (attrs.ability 1) card 1
            push $ PayCardCost iid card (defaultWindows iid)
            doStep (n - 1) msg'
      pure a
    _ -> GearedUp <$> liftRunMessage msg attrs
