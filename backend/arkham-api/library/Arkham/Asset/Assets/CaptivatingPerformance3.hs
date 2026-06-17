module Arkham.Asset.Assets.CaptivatingPerformance3 (captivatingPerformance3) where

import Arkham.Ability
import Arkham.Action (Action)
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Helpers.Action (canDo_, getActions, getCanAfford, sdrExists)
import Arkham.Helpers.Modifiers (ModifierType (..), withGrantedAction, withModifiersOf)
import Arkham.Helpers.Playable (getPlayableCards)
import Arkham.Investigator.Types (Investigator)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Window (Window (..), defaultWindows, _PerformedDifferentTypesOfActionsInARow)
import Control.Lens (_3)

newtype CaptivatingPerformance3 = CaptivatingPerformance3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

captivatingPerformance3 :: AssetCard CaptivatingPerformance3
captivatingPerformance3 = asset CaptivatingPerformance3 Cards.captivatingPerformance3

instance HasAbilities CaptivatingPerformance3 where
  getAbilities (CaptivatingPerformance3 a) =
    [ controlled a 1 (youExist CanTakeUntakenAction)
        $ triggered (PerformedDifferentTypesOfActionsInARow #after You 3 AnyAction) (exhaust a)
    ]

getStreakGroups :: [Window] -> [[Action]]
getStreakGroups = concatMap ((^. _PerformedDifferentTypesOfActionsInARow . _3) . windowType)

instance RunMessage CaptivatingPerformance3 where
  runMessage msg a@(CaptivatingPerformance3 attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      resolveAbilityAndThen (Just $ AbilityRef (toSource attrs) 1) $ doStep 1 msg
      pure a
    DoStep 1 (UseCardAbility iid (isSource attrs -> True) 1 (getStreakGroups -> groups) _) -> do
      -- A candidate action may count as several types at once (e.g. a weapon's
      -- "[action]: Fight" is both an activate and a fight action). It is a valid
      -- "different type" fourth action only if it is disjoint from at least one
      -- complete SDR of the streak, i.e. the streak still admits an SDR once the
      -- candidate's types are removed from every group.
      let canTake xs = sdrExists (map (filter (`notElem` xs)) groups)
      a' <- getAttrs @Investigator iid
      actions <- withGrantedAction iid attrs do
        filter (\x -> notNull x.actions && canTake x.actions) <$> getActions iid (defaultWindows iid)
      playableCards <-
        filterCards (not_ FastCard) <$> getPlayableCards (attrs.ability 1) iid (UnpaidCost NoAction) (defaultWindows iid)
      (resourceOk, drawOk) <- withModifiersOf iid attrs [ActionCostOf IsAnyAction (-1)] do
        (,)
          <$> andM [pure $ canTake [#resource], canDo_ iid #resource, getCanAfford a' [#resource]]
          <*> andM [pure $ canTake [#draw], canDo_ iid #draw, getCanAfford a' [#draw]]
      playOk <- andM [pure $ canTake [#play], canDo_ iid #play]

      chooseOneM iid do
        for_ actions \ab -> abilityLabeled iid (decrease_ ab 1) nothing
        when resourceOk $ resourceLabeled iid $ gainResources iid a' 1
        when drawOk $ deckLabeled iid $ drawCards iid a' 1
        when playOk $ targets playableCards $ playCardPayingCost iid
      pure a
    _ -> CaptivatingPerformance3 <$> liftRunMessage msg attrs
