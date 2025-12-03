{-# OPTIONS_GHC -Wno-deprecations #-}

module Arkham.Campaigns.TheScarletKeys.Key.Cards.TheLightOfPharos (theLightOfPharos) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Key.Cards qualified as Cards
import Arkham.Campaigns.TheScarletKeys.Key.Import.Lifted
import Arkham.Campaigns.TheScarletKeys.Key.Types
import Arkham.Card
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Helpers.Playable (getIsPlayable, withReducedCost)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher hiding (key)
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Window (defaultWindows)
import Control.Monad.State.Strict (execStateT, put)

newtype TheLightOfPharos = TheLightOfPharos ScarletKeyAttrs
  deriving anyclass (IsScarletKey, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theLightOfPharos :: ScarletKeyCard TheLightOfPharos
theLightOfPharos = key TheLightOfPharos Cards.theLightOfPharos

instance HasAbilities TheLightOfPharos where
  getAbilities (TheLightOfPharos a) = case a.bearer of
    InvestigatorTarget iid
      | not a.shifted ->
          [ restricted
              a
              1
              ( youExist (InvestigatorWithId iid)
                  <> PlayableCardExistsWithCostReduction (Reduce 3) (inHandOf ForPlay iid)
              )
              $ FastAbility Free
          ]
    ScenarioTarget
      | not a.shifted ->
          [ restricted a 1 (PlayableCardExistsWithCostReduction (Reduce 3) $ InHandOf ForPlay You)
              $ FastAbility Free
          ]
    _ -> []

instance RunMessage TheLightOfPharos where
  runMessage msg k@(TheLightOfPharos attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- store the investigator who triggered the key in case it is not the bearer
      liftRunMessage
        (CampaignSpecific "shift[09659]" Null)
        (TheLightOfPharos $ attrs {keyMeta = toJSON iid})
    CampaignSpecific "shift[09659]" _ -> do
      shiftKey attrs do
        when attrs.unstable do
          ok <- flip execStateT True do
            eachInvestigator \iid -> do
              discardableCards <- fieldMap InvestigatorHand (count (`cardMatch` DiscardableCard)) iid
              when (discardableCards < 2) $ put False
              randomDiscardN iid attrs 2

          when ok $ withInvestigatorBearer attrs (`flipOver` attrs)
        when attrs.stable do
          withInvestigatorBearer attrs \iid -> do
            cards <- withReducedCost iid attrs 3 do
              filterM
                (getIsPlayable iid GameSource (UnpaidCost NoAction) (defaultWindows iid))
                =<< select (inHandOf NotForPlay iid)

            chooseTargetM iid cards \card -> do
              reduceCostOf attrs card 3
              playCardPayingCost iid card
            flipOver iid attrs
      pure k
    _ -> TheLightOfPharos <$> liftRunMessage msg attrs
