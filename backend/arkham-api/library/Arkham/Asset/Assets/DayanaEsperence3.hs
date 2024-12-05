module Arkham.Asset.Assets.DayanaEsperence3 (dayanaEsperence3, DayanaEsperence3 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Matcher hiding (EventCard, PlaceUnderneath, PlayCard)
import Arkham.Prelude
import Arkham.Window (mkAfter, mkWhen)
import Arkham.Window qualified as Window

newtype DayanaEsperence3 = DayanaEsperence3 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dayanaEsperence3 :: AssetCard DayanaEsperence3
dayanaEsperence3 = ally DayanaEsperence3 Cards.dayanaEsperence3 (3, 1)

instance HasModifiersFor DayanaEsperence3 where
  getModifiersFor (DayanaEsperence3 a) = do
    investigator <- case a.controller of
      Nothing -> pure mempty
      Just iid -> modified_ a iid $ map AsIfInHand $ assetCardsUnderneath a

    cards <-
      modifyEach
        a
        (assetCardsUnderneath a)
        [LeaveCardWhereItIs, AdditionalCost $ assetUseCost a Secret 1 <> exhaust a]

    pure $ investigator <> cards

instance HasAbilities DayanaEsperence3 where
  getAbilities (DayanaEsperence3 a) =
    [ controlled a 1 (exists $ InHandOf You <> basic (NonWeakness <> #spell <> #event)) $ FastAbility Free
    ]

instance RunMessage DayanaEsperence3 where
  runMessage msg a@(DayanaEsperence3 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      cards <- select $ inHandOf iid <> basic (NonWeakness <> #spell <> #event)

      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ targetLabel
            (toCardId c)
            ( PlaceUnderneath (toTarget attrs) [c]
                : map
                  (\other -> AddToDiscard (fromMaybe iid $ toCardOwner other) other)
                  (onlyPlayerCards $ assetCardsUnderneath attrs)
            )
          | c <- cards
          ]
      pure a
    InitiatePlayCard iid card mTarget payment windows' asAction | card `elem` assetCardsUnderneath attrs -> do
      afterPlayCard <- checkWindows [mkAfter (Window.PlayCard iid $ Window.CardPlay card asAction)]
      if cdSkipPlayWindows (toCardDef card)
        then push $ PlayCard iid card mTarget payment windows' asAction
        else
          pushAll
            [ CheckWindows [mkWhen (Window.PlayCard iid $ Window.CardPlay card asAction)]
            , PlayCard iid card mTarget payment windows' asAction
            , afterPlayCard
            ]
      pure a
    _ -> DayanaEsperence3 <$> runMessage msg attrs
