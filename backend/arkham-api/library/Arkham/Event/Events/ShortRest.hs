module Arkham.Event.Events.ShortRest (shortRest) where

import Arkham.Ability
import Arkham.Card
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Healing
import Arkham.Matcher

newtype ShortRest = ShortRest EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shortRest :: EventCard ShortRest
shortRest = event ShortRest Cards.shortRest

instance HasAbilities ShortRest where
  getAbilities (ShortRest a) =
    [ restricted a 1 InYourHand
        $ freeReaction
          (Arkham.Matcher.PlayEventDiscarding #after (colocatedWithMatch You) (eventIs Cards.shortRest))
    ]

instance RunMessage ShortRest where
  runMessage msg e@(ShortRest attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      investigators <-
        select
          $ oneOf [HealableInvestigator (toSource attrs) kind (colocatedWith iid) | kind <- [#damage, #horror]]
      assets <-
        select
          $ oneOf [HealableAsset (toSource attrs) kind (assetAtLocationWith iid) | kind <- [#damage, #horror]]
      chooseOneM iid do
        targets investigators $ handleTarget iid attrs
        targets assets $ handleTarget iid attrs
      pure e
    HandleTargetChoice _iid (isSource attrs -> True) (InvestigatorTarget iid') -> do
      chooseHealDamageOrHorror attrs iid'
      pure e
    HandleTargetChoice iid (isSource attrs -> True) (AssetTarget aid) -> do
      assetChooseHealDamageOrHorror attrs iid aid
      pure e
    InHand _ (UseThisAbility iid (isSource attrs -> True) 1) -> do
      playCardPayingCost iid (toCard attrs)
      pure e
    _ -> ShortRest <$> liftRunMessage msg attrs
