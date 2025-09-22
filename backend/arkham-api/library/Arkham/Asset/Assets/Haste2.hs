module Arkham.Asset.Assets.Haste2 (haste2) where

import Arkham.Ability
import Arkham.Action (Action)
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Helpers.Action (canDo, getActions, getCanAfford)
import Arkham.Helpers.Modifiers (ModifierType (..), withGrantedAction, withModifiersOf)
import Arkham.Helpers.Playable (getPlayableCards)
import Arkham.Investigator.Types (Investigator)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Window (Window (..), defaultWindows, _PerformedSameTypeOfAction)
import Control.Lens (_2)

newtype Haste2 = Haste2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

haste2 :: AssetCard Haste2
haste2 = asset Haste2 Cards.haste2

instance HasAbilities Haste2 where
  getAbilities (Haste2 a) =
    [ restricted a 1 ControlsThis
        $ triggered (PerformedSameTypeOfAction #after You RepeatableAction) (exhaust a)
    ]

getActionTypes :: [Window] -> [Action]
getActionTypes = concatMap ((^. _PerformedSameTypeOfAction . _2) . windowType)

instance RunMessage Haste2 where
  runMessage msg a@(Haste2 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getActionTypes -> as) _ -> do
      a' <- getAttrs @Investigator iid
      actions <- withGrantedAction iid attrs do
        filter (\x -> any (abilityIs x) as) <$> getActions iid (defaultWindows iid)
      canPlay <- canDo iid #play
      let cardF = if #play `elem` as then id else (<> mapOneOf CardWithAction as)
      playableCards <-
        if canPlay
          then
            filterCards (cardF (not_ FastCard))
              <$> getPlayableCards (attrs.ability 1) iid (UnpaidCost NoAction) (defaultWindows iid)
          else
            pure []

      (resourceOk, drawOk) <- withModifiersOf iid attrs [ActionCostOf IsAnyAction (-1)] do
        (,)
          <$> andM [pure $ #resource `elem` as, canDo iid #resource, getCanAfford a' [#resource]]
          <*> andM [pure $ #draw `elem` as, canDo iid #draw, getCanAfford a' [#draw]]

      chooseOneM iid do
        for_ actions \ab -> abilityLabeled iid (decrease_ ab 1) nothing
        when resourceOk $ resourceLabeled iid $ gainResources iid a' 1
        when drawOk $ deckLabeled iid $ drawCards iid a' 1
        targets playableCards $ playCardPayingCost iid
      pure a
    _ -> Haste2 <$> liftRunMessage msg attrs
