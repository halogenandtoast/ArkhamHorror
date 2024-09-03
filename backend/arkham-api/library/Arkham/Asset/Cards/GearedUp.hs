module Arkham.Asset.Cards.GearedUp (gearedUp, GearedUp (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Game.Helpers (getPlayableCards)
import Arkham.Helpers.Modifiers (withModifiers)
import Arkham.Helpers.Modifiers qualified as Msg
import Arkham.Investigator.Types (Investigator)
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Projection
import Arkham.Taboo
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
      iattrs <- getAttrs @Investigator iid
      cards <- withModifiers iid (toModifiers attrs [ReduceCostOf AnyCard 1]) $ do
        filter (`cardMatch` card_ (#asset <> #item))
          <$> getPlayableCards iattrs (UnpaidCost NoAction) (defaultWindows iid)
      when (notNull cards) do
        ( if tabooed TabooList21 attrs
            then chooseUpToN iid 5 "Done Playing Items"
            else chooseOne iid . (Label "Done Playing Items" [] :)
          )
          [ targetLabel
            (toCardId card)
            [ Msg.reduceCostOf (attrs.ability 1) card 1
            , PayCardCost iid card (defaultWindows iid)
            , DoStep 1 msg'
            ]
          | card <- cards
          ]
      pure a
    _ -> GearedUp <$> liftRunMessage msg attrs
