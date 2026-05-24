module Arkham.Act.Cards.SearchingTheHeath (searchingTheHeath) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.Helpers.Location (withLocationOf)
import Arkham.I18n
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection

newtype SearchingTheHeath = SearchingTheHeath ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

searchingTheHeath :: ActCard SearchingTheHeath
searchingTheHeath = act (2, A) SearchingTheHeath Cards.searchingTheHeath Nothing

crystalRemainsMatcher :: CardMatcher
crystalRemainsMatcher =
  mapOneOf
    cardIs
    [Assets.crystalRemainsTheChild, Assets.crystalRemainsTheFather, Assets.crystalRemainsTheMother]

instance HasAbilities SearchingTheHeath where
  getAbilities = actAbilities \a ->
    [ restricted
        a
        1
        (exists $ You <> at_ (LocationWithoutClues <> LocationWithCardsUnderneath AnyCards))
        actionAbility
    , restricted a 2 (exists $ You <> at_ (LocationWithCardsUnderneath AnyCards))
        $ freeTrigger (GroupClueCost (PerPlayer 1) YourLocation)
    , restricted
        a
        3
        ( AllUndefeatedInvestigatorsResigned
            <> exists (VictoryDisplayCardMatch $ basic crystalRemainsMatcher)
        )
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage SearchingTheHeath where
  runMessage msg a@(SearchingTheHeath attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withLocationOf iid \loc -> do
        cards <- fieldMap LocationCardsUnderneath (take 1) loc
        revealed <- traverse (setFacedown False) cards
        for_ revealed (drawCard iid)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      withLocationOf iid \loc -> do
        cards <- fieldMap LocationCardsUnderneath (take 1) loc
        revealed <- traverse (setFacedown False) cards
        focusCards revealed do
          chooseOneM iid $ withI18n do
            labeled' "placeOnBottom" do
              unfocusCards
              for_ cards \card -> do
                forTarget' card $ doStep 1 msg
                obtainCard card
                placeUnderneath loc [card]
            labeled' "leaveOnTop" do
              unfocusCards
              for_ cards \card ->
                forTarget' card $ doStep 1 msg
      pure a
    ForTarget (CardIdTarget cid) (DoStep 1 (UseThisAbility _ (isSource attrs -> True) 2)) -> do
      card <- fetchCard cid
      void $ setFacedown True card
      pure a
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      n <- selectCount $ VictoryDisplayCardMatch $ basic crystalRemainsMatcher
      push $ if n == 3 then R1 else R2
      pure a
    _ -> SearchingTheHeath <$> liftRunMessage msg attrs
