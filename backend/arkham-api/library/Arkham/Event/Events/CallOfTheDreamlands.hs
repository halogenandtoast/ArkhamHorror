module Arkham.Event.Events.CallOfTheDreamlands (callOfTheDreamlands) where

import Arkham.Card
import Arkham.Cost.Status
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Cost (getSpendableResources)
import Arkham.Helpers.Dreaming (getDreamingValue)
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.Playable (getIsPlayable, getIsPlayableWithResources)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Strategy
import Arkham.Trait (Trait (Dreamlands))
import Arkham.Window (mkWhen)
import Arkham.Window qualified as Window

newtype CallOfTheDreamlands = CallOfTheDreamlands EventAttrs
  deriving anyclass (IsEvent, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

callOfTheDreamlands :: EventCard CallOfTheDreamlands
callOfTheDreamlands = event CallOfTheDreamlands Cards.callOfTheDreamlands

instance RunMessage CallOfTheDreamlands where
  runMessage msg e@(CallOfTheDreamlands attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      search iid attrs iid [fromTopOfDeck 9] (basic $ withTrait Dreamlands) (defer attrs IsNotDraw)
      pure e
    SearchNoneFound iid (isTarget attrs -> True) -> do
      shuffleDeck iid
      pure e
    SearchFound iid (isTarget attrs -> True) _ cards -> do
      -- the windows the search itself would have played the card in
      let ws = [mkWhen Window.NonFast, mkWhen (Window.DuringTurn iid)]
      resources <- getSpendableResources iid
      options <- forMaybeM cards \card -> do
        {- The branch set comes from the card, never offered blind: a dreaming card
        has no resource cost and a plain Dreamlands card has no dreaming value, so
        an illegal branch would be a criteria-gated option that cannot be logged
        (#6017). Both are legal only where something granted dreaming to a costed
        card. -}
        mDreaming <- getDreamingValue card
        playableDiscounted <-
          getIsPlayableWithResources iid attrs (resources + 1) (UnpaidCost NoAction) ws card
        playableAsIs <- getIsPlayable iid attrs (UnpaidCost NoAction) ws card
        let hasResourceCost = isJust (toCardDef card).cost
        let reduceCost = ReduceCostOf (CardWithId card.id) 1
        let branches =
              [("reduceCost", reduceCost) | hasResourceCost, playableDiscounted]
                <> [("reduceDreaming", ReduceDreaming 1) | isJust mDreaming, playableAsIs]
        pure $ if null branches then Nothing else Just (card, branches)

      -- one TargetLabel per card: a second with the same card id is unclickable,
      -- so the reduction is a nested ask. It is always an ask, even at one option.
      when (notNull options) do
        chooseOneM iid $ for_ options \(card, branches) ->
          targeting card
            $ chooseOneM iid
            $ cardI18n
            $ scope "callOfTheDreamlands"
            $ for_ branches \(key, modifier) -> labeled key do
              costModifier attrs card modifier
              playCardPayingCostWithWindows iid card ws
      shuffleDeck iid
      pure e
    _ -> CallOfTheDreamlands <$> liftRunMessage msg attrs
