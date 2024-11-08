module Arkham.Event.Events.UncageTheSoul3 (uncageTheSoul3, UncageTheSoul3 (..)) where

import Arkham.Asset.Types (Field (..))
import Arkham.Cost
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Game.Helpers (getIsPlayableWithResources)
import Arkham.Helpers.Cost (getSpendableResources)
import Arkham.Matcher hiding (AssetCard, PlayCard)
import Arkham.Projection
import Arkham.Window (defaultWindows)

newtype UncageTheSoul3 = UncageTheSoul3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

uncageTheSoul3 :: EventCard UncageTheSoul3
uncageTheSoul3 = event UncageTheSoul3 Cards.uncageTheSoul3

instance RunMessage UncageTheSoul3 where
  runMessage msg e@(UncageTheSoul3 attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      assets <- select $ assetControlledBy iid <> DiscardableAsset <> oneOf [#spell, #ritual]
      rs <- (+ 3) <$> getSpendableResources iid
      assets' <- flip filterM assets \aid -> do
        card <- field AssetCard aid
        getIsPlayableWithResources iid attrs rs (UnpaidCost NoAction) (defaultWindows iid) card
      results <-
        select
          $ oneOf
            [ inHandOf iid <> basic (oneOf [#spell, #ritual])
            , inDiscardOf iid <> basic (oneOf [#spell, #ritual])
            ]
      if null results
        then chooseTargetM iid assets' $ toDiscardBy iid attrs
        else unless (null assets) do
          chooseOneM iid do
            labeled "Do not discard asset" nothing
            targets assets $ toDiscardBy iid attrs
      doStep 1 msg
      pure e
    DoStep 1 (PlayThisEvent iid eid) | eid == toId attrs -> do
      let ws = defaultWindows iid
      rs <- (+ 3) <$> getSpendableResources iid

      cards <-
        filterM (getIsPlayableWithResources iid GameSource rs (UnpaidCost NoAction) ws)
          =<< select
            ( oneOf
                [ inHandOf iid <> basic (oneOf [#spell, #ritual])
                , inDiscardOf iid <> basic (oneOf [#spell, #ritual])
                ]
            )
      chooseTargetM iid cards \c -> do
        addToHandQuiet iid (only c)
        reduceCostOf attrs c 3
        playCardPayingCost iid c
      pure e
    _ -> UncageTheSoul3 <$> liftRunMessage msg attrs
