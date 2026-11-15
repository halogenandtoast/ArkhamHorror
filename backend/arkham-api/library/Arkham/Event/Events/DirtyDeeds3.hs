module Arkham.Event.Events.DirtyDeeds3 (dirtyDeeds3, DirtyDeeds3 (..)) where

import Arkham.Ability
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Strategy
import Arkham.Window (defaultWindows)

newtype DirtyDeeds3 = DirtyDeeds3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dirtyDeeds3 :: EventCard DirtyDeeds3
dirtyDeeds3 = event DirtyDeeds3 Cards.dirtyDeeds3

instance RunMessage DirtyDeeds3 where
  runMessage msg e@(DirtyDeeds3 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      search
        iid
        (attrs.ability 1)
        iid
        [fromDeck]
        (PlayableCard (UnpaidCost NoAction) $ #illicit <> #asset)
        (defer attrs IsNotDraw)
      pure e
    SearchFound iid (isTarget attrs -> True) _ cards -> do
      -- playCardPayingCostWithWindows, not a bare PayCardCost: the bare message opens
      -- no PlayCard window, so "when you play an [[Item]] asset" (Shrewd Dealings) never
      -- saw the asset this card plays (#5998).
      chooseOneM iid $ targets cards \card -> do
        addToHand iid (only card)
        playCardPayingCostWithWindows iid card (defaultWindows iid)
        handleTarget iid attrs card
      pure e
    SearchNoneFound iid (isTarget attrs -> True) -> do
      chooseOne iid [Label "$label.noCardsFound" []]
      pure e
    HandleTargetChoice iid (isSource attrs -> True) (CardIdTarget cid) -> do
      abilities <-
        map ((`applyAbilityModifiers` [IgnoreAllCosts]) . doesNotProvokeAttacksOfOpportunity)
          <$> select
            ( oneOf [AbilityIsActionAbility, AbilityIsFastAbility]
                <> AbilityOnAsset (AssetWithCardId cid)
                <> PerformableAbility [IgnoreAllCosts]
            )
      -- "you *may* resolve" -- chooseOrRunOne auto-runs the lone ability, which forced
      -- Liquid Courage's [action] (and its willpower test) on the player (#5998).
      when (notNull abilities) do
        chooseOne iid
          $ Label "$label.doNotUseAbility" []
          : [AbilityLabel iid ab (defaultWindows iid) [] [] | ab <- abilities]
      pure e
    _ -> DirtyDeeds3 <$> liftRunMessage msg attrs
