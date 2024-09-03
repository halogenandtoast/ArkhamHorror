module Arkham.Event.Cards.WordOfWoe (wordOfWoe, WordOfWoe (..)) where

import Arkham.Ability
import Arkham.Deck
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Game.Helpers (getCanPerformAbility)
import Arkham.Helpers.Message (handleTargetChoice)
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Window (defaultWindows)

newtype WordOfWoe = WordOfWoe EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wordOfWoe :: EventCard WordOfWoe
wordOfWoe = event WordOfWoe Cards.wordOfWoe

instance RunMessage WordOfWoe where
  runMessage msg e@(WordOfWoe attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      assets <-
        select
          $ assetControlledBy iid
          <> AssetWithPerformableAbility AbilityIsActionAbility [IgnoreAllCosts]
      chooseOrRunOne iid $ targetLabels assets $ only . handleTargetChoice iid attrs
      pure e
    HandleTargetChoice iid (isSource attrs -> True) (AssetTarget aid) -> do
      let adjustAbility ab = applyAbilityModifiers ab [IgnoreAllCosts]
      abilities <-
        selectMap (doesNotProvokeAttacksOfOpportunity . adjustAbility)
          $ AssetAbility (AssetWithId aid)
          <> AbilityIsActionAbility
      abilities' <- filterM (getCanPerformAbility iid (defaultWindows iid)) abilities
      placeDoom attrs aid 1
      chooseOne iid [AbilityLabel iid ab [] [] [DoStep 1 msg] | ab <- abilities']
      pure e
    DoStep 1 (HandleTargetChoice iid (isSource attrs -> True) _) -> do
      wordOfWeal <- selectOne $ inDiscardOf iid <> basic (cardIs Cards.wordOfWeal)

      for_ wordOfWeal \card -> do
        chooseOne
          iid
          [ Label "Shuffle Word of Weal into your deck" [ShuffleCardsIntoDeck (toDeck iid) [card]]
          , Label "Do not shuffle it back in" []
          ]

      pure e
    _ -> WordOfWoe <$> liftRunMessage msg attrs
