module Arkham.Event.Cards.DirtyDeeds3 (dirtyDeeds3, DirtyDeeds3 (..)) where

import Arkham.Ability
import Arkham.Card
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Message (handleTargetChoice)
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
      chooseOne
        iid
        [ targetLabel
          (toCardId card)
          [AddToHand iid [card], PayCardCost iid card (defaultWindows iid), handleTargetChoice iid attrs card]
        | card <- cards
        ]
      pure e
    SearchNoneFound iid (isTarget attrs -> True) -> do
      chooseOne iid [Label "No Cards Found" []]
      pure e
    HandleTargetChoice iid (isSource attrs -> True) (CardIdTarget cid) -> do
      abilities <-
        map (`applyAbilityModifiers` [IgnoreAllCosts])
          <$> select
            ( PerformableAbility [IgnoreAllCosts]
                <> oneOf [AbilityIsActionAbility, AbilityIsFastAbility]
                <> AbilityOnAsset (AssetWithCardId cid)
            )
      when (notNull abilities) do
        chooseOrRunOne iid [AbilityLabel iid ab [] [] [] | ab <- abilities]
      pure e
    _ -> DirtyDeeds3 <$> liftRunMessage msg attrs
