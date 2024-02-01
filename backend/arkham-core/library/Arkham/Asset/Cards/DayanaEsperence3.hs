module Arkham.Asset.Cards.DayanaEsperence3 (
  dayanaEsperence3,
  DayanaEsperence3 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Matcher hiding (EventCard, PlaceUnderneath, PlayCard)
import Arkham.Window (mkAfter, mkWhen)
import Arkham.Window qualified as Window

newtype DayanaEsperence3 = DayanaEsperence3 AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

dayanaEsperence3 :: AssetCard DayanaEsperence3
dayanaEsperence3 = ally DayanaEsperence3 Cards.dayanaEsperence3 (3, 1)

instance HasModifiersFor DayanaEsperence3 where
  getModifiersFor (InvestigatorTarget iid) (DayanaEsperence3 attrs)
    | controlledBy attrs iid = do
        pure $ toModifiers attrs [AsIfInHand card | card <- assetCardsUnderneath attrs]
  getModifiersFor (CardIdTarget cardId) (DayanaEsperence3 attrs) = do
    pure
      $ toModifiers attrs
      $ guard (cardId `elem` map toCardId (assetCardsUnderneath attrs))
      *> [ LeaveCardWhereItIs
         , AdditionalCost $ assetUseCost attrs Secret 1 <> exhaust attrs
         ]
  getModifiersFor _ _ = pure []

instance HasAbilities DayanaEsperence3 where
  getAbilities (DayanaEsperence3 a) =
    [ controlledAbility
        a
        1
        (ExtendedCardExists $ InHandOf You <> BasicCardMatch (NonWeakness <> #spell <> #event))
        $ FastAbility Free
    ]

instance RunMessage DayanaEsperence3 where
  runMessage msg a@(DayanaEsperence3 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      cards <- selectList $ InHandOf You <> BasicCardMatch (NonWeakness <> #spell <> #event)

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
    InitiatePlayCard iid card mTarget windows' asAction | card `elem` assetCardsUnderneath attrs -> do
      afterPlayCard <- checkWindows [mkAfter (Window.PlayCard iid card)]
      if cdSkipPlayWindows (toCardDef card)
        then push $ PlayCard iid card mTarget windows' asAction
        else
          pushAll
            [ CheckWindow [iid] [mkWhen (Window.PlayCard iid card)]
            , PlayCard iid card mTarget windows' asAction
            , afterPlayCard
            ]
      pure a
    _ -> DayanaEsperence3 <$> runMessage msg attrs
