module Arkham.Asset.Assets.UnderworldMarket2 (underworldMarket2) where

import Arkham.Ability
import Arkham.Message.Lifted.Choose
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers
import Arkham.Helpers.Cost (getSpendableResources)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection

newtype Meta = Meta {marketDeck :: [Card]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype UnderworldMarket2 = UnderworldMarket2 (AssetAttrs `With` Meta)
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

underworldMarket2 :: AssetCard UnderworldMarket2
underworldMarket2 = asset (UnderworldMarket2 . (`with` Meta [])) Cards.underworldMarket2

instance HasAbilities UnderworldMarket2 where
  getAbilities (UnderworldMarket2 (With attrs meta)) =
    [ restricted attrs 1 ControlsThis $ freeReaction $ DrawingStartingHand #when You
    , controlled attrs 2 criteria $ freeReaction $ TurnBegins #when You
    ]
   where
    criteria = if null (marketDeck meta) then Never else NoRestriction

instance RunMessage UnderworldMarket2 where
  runMessage msg a@(UnderworldMarket2 (With attrs meta)) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      xs <- filterCards (card_ #illicit) <$> fieldMap InvestigatorDeck (map toCard . unDeck) iid
      focusCards xs $ chooseNM iid 10 $ targets xs $ handleTarget iid (attrs.ability 1)
      pure a
    HandleTargetChoice _iid (isAbilitySource attrs 1 -> True) (CardIdTarget cid) -> do
      card <- getCard cid
      obtainCard card
      deck' <- shuffle (card : marketDeck meta)
      pure . UnderworldMarket2 . (`with` Meta deck') $ attrs
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      let (xs, rest) = splitAt 2 $ marketDeck meta
      when (notNull xs) do
        focusCards xs do
          spendableResources <- getSpendableResources iid
          chooseOneM iid do
            labeled "Place the rest on the bottom, in any order" do
              chooseOneAtATimeM iid $ targets xs $ handleTarget iid (attrs.ability 2)
              unfocusCards
            when (spendableResources > 0) do
              labeled "Spend 1 resource to draw 1 of them" do
                push $ SpendResources iid 1
                chooseOneM iid do
                  for_ (eachWithRest xs) \(card, cs) -> do
                    targeting card do
                      unfocusCards
                      addToHand iid (only card)
                      focusCards cs do
                        chooseOrRunOneAtATimeM iid $ targets cs $ handleTarget iid (attrs.ability 2)
                        unfocusCards

      pure . UnderworldMarket2 . (`with` Meta rest) $ attrs
    HandleTargetChoice _iid (isAbilitySource attrs 2 -> True) (CardIdTarget cid) -> do
      card <- getCard cid
      pure . UnderworldMarket2 . (`with` Meta (marketDeck meta ++ [card])) $ attrs
    _ -> UnderworldMarket2 . (`with` meta) <$> liftRunMessage msg attrs
