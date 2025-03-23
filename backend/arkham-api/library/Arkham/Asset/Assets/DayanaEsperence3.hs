module Arkham.Asset.Assets.DayanaEsperence3 (dayanaEsperence3) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Card
import Arkham.Helpers.Modifiers
import Arkham.Matcher hiding (EventCard, PlaceUnderneath, PlayCard)
import Arkham.Message.Lifted.Choose
import Arkham.Window qualified as Window

newtype DayanaEsperence3 = DayanaEsperence3 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dayanaEsperence3 :: AssetCard DayanaEsperence3
dayanaEsperence3 = ally DayanaEsperence3 Cards.dayanaEsperence3 (3, 1)

instance HasModifiersFor DayanaEsperence3 where
  getModifiersFor (DayanaEsperence3 a) = do
    controllerGets a $ map AsIfInHand $ assetCardsUnderneath a

    modifyEach
      a
      (assetCardsUnderneath a)
      [LeaveCardWhereItIs, AdditionalCost $ assetUseCost a Secret 1 <> exhaust a]

instance HasAbilities DayanaEsperence3 where
  getAbilities (DayanaEsperence3 a) =
    [ controlled a 1 (exists $ InHandOf ForPlay You <> basic (NonWeakness <> #spell <> #event))
        $ FastAbility Free
    ]

instance RunMessage DayanaEsperence3 where
  runMessage msg a@(DayanaEsperence3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      cards <- select $ inHandOf ForPlay iid <> basic (NonWeakness <> #spell <> #event)

      chooseTargetM iid cards \c -> do
        placeUnderneath attrs (only c)
        for_ (onlyPlayerCards $ assetCardsUnderneath attrs) \other ->
          addToDiscard (fromMaybe iid other.owner) (only other)
      pure a
    InitiatePlayCard iid card mTarget payment windows' asAction | card `elem` assetCardsUnderneath attrs -> do
      if cdSkipPlayWindows (toCardDef card)
        then push $ PlayCard iid card mTarget payment windows' asAction
        else do
          checkWhen $ Window.PlayCard iid $ Window.CardPlay card asAction
          push $ PlayCard iid card mTarget payment windows' asAction
          checkAfter $ Window.PlayCard iid $ Window.CardPlay card asAction
      pure a
    CardEnteredPlay _ card | card `elem` assetCardsUnderneath attrs -> pure a
    _ -> DayanaEsperence3 <$> liftRunMessage msg attrs
