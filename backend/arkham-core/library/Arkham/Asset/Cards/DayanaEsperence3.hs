module Arkham.Asset.Cards.DayanaEsperence3 (
  dayanaEsperence3,
  DayanaEsperence3 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Event.Types (Field (EventCard))
import Arkham.Matcher hiding (EventCard)
import Arkham.Placement
import Arkham.Trait (Trait (Spell))

newtype DayanaEsperence3 = DayanaEsperence3 AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dayanaEsperence3 :: AssetCard DayanaEsperence3
dayanaEsperence3 = ally DayanaEsperence3 Cards.dayanaEsperence3 (3, 1)

instance HasModifiersFor DayanaEsperence3 where
  getModifiersFor (InvestigatorTarget iid) (DayanaEsperence3 attrs)
    | controlledBy attrs iid = do
        events <-
          selectWithField EventCard $
            EventAttachedToAsset $
              AssetWithId $
                toId
                  attrs
        pure $ toModifiers attrs [AsIfInHand card | (_, card) <- events]
  getModifiersFor (CardIdTarget cardId) (DayanaEsperence3 attrs) = do
    events <-
      selectWithField EventCard $
        EventAttachedToAsset $
          AssetWithId $
            toId
              attrs

    pure $
      toModifiers
        attrs
        [ AdditionalCost $
          UseCost (AssetWithId $ toId attrs) Secret 1
            <> ExhaustCost (toTarget attrs)
        | (_, card) <- events
        , toCardId card == cardId
        ]
  getModifiersFor _ _ = pure []

instance HasAbilities DayanaEsperence3 where
  getAbilities (DayanaEsperence3 a) =
    [ restrictedAbility
        a
        1
        ( ControlsThis
            <> ExtendedCardExists
              ( InHandOf You
                  <> BasicCardMatch
                    (NonWeakness <> CardWithTrait Spell <> CardWithType EventType)
              )
        )
        $ FastAbility Free
    ]

instance RunMessage DayanaEsperence3 where
  runMessage msg a@(DayanaEsperence3 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      cards <-
        selectList $
          InHandOf You
            <> BasicCardMatch
              (NonWeakness <> CardWithTrait Spell <> CardWithType EventType)

      push $
        chooseOne
          iid
          [ targetLabel
            (toCardId c)
            [ RemoveCardFromHand iid (toCardId c)
            , CreateEventAt iid c $
                AttachedToAsset (toId attrs) (Just $ StillInHand iid)
            ]
          | c <- cards
          ]
      pure a
    InitiatePlayCard iid card mTarget windows' _ -> do
      events <-
        selectWithField EventCard $
          EventAttachedToAsset $
            AssetWithId $
              toId
                attrs
      -- we place again to remove the card from the investigator's events
      for_ (find ((== card) . snd) events) $ \(event, card') ->
        pushAll
          [ PayCardCost iid card' windows'
          , InvestigatorPlayEvent iid event mTarget windows' FromHand
          , PlaceEvent iid event $ AttachedToAsset (toId attrs) (Just $ StillInHand iid)
          ]
      pure a
    _ -> DayanaEsperence3 <$> runMessage msg attrs
