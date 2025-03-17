module Arkham.Asset.Assets.CaptivatingPerformance3 (captivatingPerformance3) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Helpers.Action (canDo, getActions, getCanAfford)
import Arkham.Helpers.Modifiers (ModifierType (..), withGrantedAction, withModifiersOf)
import Arkham.Helpers.Playable (getPlayableCards)
import Arkham.Investigator.Types (Field (..), Investigator)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Window (defaultWindows)

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

instance RunMessage CaptivatingPerformance3 where
  runMessage msg a@(CaptivatingPerformance3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      as' <- nub . concat <$> field InvestigatorActionsTaken iid
      let as = filter (`notElem` as') [minBound ..]
      a' <- getAttrs @Investigator iid
      actions <- withGrantedAction iid attrs do
        filter (\x -> any (abilityIs x) as) <$> getActions iid (defaultWindows iid)
      playableCards <-
        filterCards (not_ FastCard) <$> getPlayableCards (attrs.ability 1) iid (UnpaidCost NoAction) (defaultWindows iid)
      (resourceOk, drawOk) <- withModifiersOf iid attrs [ActionCostOf IsAnyAction (-1)] do
        (,)
          <$> andM [pure $ #resource `elem` as, canDo iid #resource, getCanAfford a' [#resource]]
          <*> andM [pure $ #draw `elem` as, canDo iid #draw, getCanAfford a' [#draw]]
      playOk <- andM [pure $ #play `elem` as, canDo iid #play]

      chooseOneM iid do
        for_ actions \ab -> abilityLabeled iid (decrease_ ab 1) nothing
        when resourceOk $ resourceLabeled iid $ gainResources iid a' 1
        when drawOk $ deckLabeled iid $ drawCards iid a' 1
        when playOk $ targets playableCards $ playCardPayingCost iid
      pure a
    _ -> CaptivatingPerformance3 <$> liftRunMessage msg attrs
