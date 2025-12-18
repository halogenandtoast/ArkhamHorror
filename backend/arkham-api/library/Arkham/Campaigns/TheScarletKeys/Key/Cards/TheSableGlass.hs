module Arkham.Campaigns.TheScarletKeys.Key.Cards.TheSableGlass (theSableGlass) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Campaigns.TheScarletKeys.Key.Cards qualified as Cards
import Arkham.Campaigns.TheScarletKeys.Key.Import.Lifted
import Arkham.Matcher hiding (key)
import Arkham.Message.Lifted.Choose
import Control.Monad.State.Strict

newtype TheSableGlass = TheSableGlass ScarletKeyAttrs
  deriving anyclass (IsScarletKey, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSableGlass :: ScarletKeyCard TheSableGlass
theSableGlass = key TheSableGlass Cards.theSableGlass

instance HasAbilities TheSableGlass where
  getAbilities (TheSableGlass a) = case a.bearer of
    InvestigatorTarget iid | not a.shifted ->
      case a.stability of
        Stable ->
          [ restricted a 1 (youExist $ InvestigatorWithId iid <> at_ LocationWithConcealedCard)
              $ FastAbility Free
          ]
        Unstable -> [restricted a 1 (youExist $ InvestigatorWithId iid) $ FastAbility Free]
    _ -> []

instance RunMessage TheSableGlass where
  runMessage msg k@(TheSableGlass attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> liftRunMessage (CampaignSpecific "shift[09634]" Null) k
    CampaignSpecific "shift[09634]" _ -> do
      shiftKey attrs do
        when attrs.unstable do
          ok <- flip execStateT True do
            eachInvestigator \iid -> do
              cards <- select $ inHandOf NotForPlay iid <> basic NonWeakness
              let highestCost = maxesBy (.printedCost) cards
              if null highestCost
                then put False
                else chooseTargetM iid highestCost $ hollow iid
          when ok $ withInvestigatorBearer attrs (`flipOver` attrs)
        when attrs.stable do
          withInvestigatorBearer attrs \iid -> do
            concealedCards <- select $ ConcealedCardAt (locationWithInvestigator iid)
            chooseOneAtATimeM iid $ targets concealedCards $ revealConcealed iid attrs . toId
            handleUnstableFlip iid attrs
      pure k
    _ -> TheSableGlass <$> liftRunMessage msg attrs
