module Arkham.Asset.Cards.UnderworldMarket2 (underworldMarket2, UnderworldMarket2 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers
import Arkham.Helpers.Cost (getSpendableResources)
import Arkham.Helpers.Query (getPlayer)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message qualified as Msg
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
  getAbilities (UnderworldMarket2 (With attrs _)) =
    [ restrictedAbility attrs 1 ControlsThis
        $ freeReaction
        $ DrawingStartingHand #when You
    , restrictedAbility attrs 2 ControlsThis $ freeReaction $ TurnBegins #when You
    ]

instance RunMessage UnderworldMarket2 where
  runMessage msg a@(UnderworldMarket2 (With attrs meta)) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      xs <- filterCards (card_ #illicit) <$> fieldMap InvestigatorDeck (map toCard . unDeck) iid

      when (notNull xs) do
        focusCards xs \unfocus -> do
          chooseN
            iid
            10
            [targetLabel card [HandleTargetChoice iid (attrs.ability 1) (toTarget card)] | card <- xs]
          push unfocus
      pure a
    HandleTargetChoice _iid (isAbilitySource attrs 1 -> True) (CardIdTarget cid) -> do
      card <- getCard cid
      push $ ObtainCard card
      deck' <- shuffleM (card : marketDeck meta)
      pure . UnderworldMarket2 . (`with` Meta deck') $ attrs
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      let (xs, rest) = splitAt 2 $ marketDeck meta
      when (notNull xs) do
        focusCards xs \unfocus -> do
          spendableResources <- getSpendableResources iid
          player <- getPlayer iid
          let placeOnBottom c = targetLabel c [HandleTargetChoice iid (attrs.ability 2) (toTarget c)]
          let choosePlaceOnBottom cs = Msg.chooseOneAtATime player $ map placeOnBottom cs
          chooseOne iid
            $ Label "Place the rest on the bottom, in any order" [choosePlaceOnBottom xs, unfocus]
            : [ Label
                "Spend 1 resource to draw 1 of them"
                [ SpendResources iid 1
                , Msg.chooseOne
                    player
                    [ targetLabel card [AddToHand iid [card], choosePlaceOnBottom cs]
                    | (card, cs) <- eachWithRest xs
                    ]
                , unfocus
                ]
              | spendableResources > 0
              ]

      pure . UnderworldMarket2 . (`with` Meta rest) $ attrs
    HandleTargetChoice _iid (isAbilitySource attrs 2 -> True) (CardIdTarget cid) -> do
      card <- getCard cid
      pure . UnderworldMarket2 . (`with` Meta (marketDeck meta ++ [card])) $ attrs
    _ -> UnderworldMarket2 . (`with` meta) <$> liftRunMessage msg attrs
