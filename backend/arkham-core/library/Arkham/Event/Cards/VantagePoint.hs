module Arkham.Event.Cards.VantagePoint
  ( vantagePoint
  , vantagePointEffect
  , VantagePoint(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.Id
import Arkham.Matcher hiding ( RevealLocation, PutLocationIntoPlay )
import Arkham.Message hiding ( RevealLocation )
import Arkham.Target
import Arkham.Window hiding ( EndTurn )

newtype VantagePoint = VantagePoint EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

vantagePoint :: EventCard VantagePoint
vantagePoint = event VantagePoint Cards.vantagePoint

vantagePointLocation :: [Window] -> LocationId
vantagePointLocation [] = error "No vantage point found"
vantagePointLocation (Window _ (PutLocationIntoPlay _ lid) : _) = lid
vantagePointLocation (Window _ (RevealLocation _ lid) : _) = lid
vantagePointLocation (_ : xs) = vantagePointLocation xs

instance RunMessage VantagePoint where
  runMessage msg e@(VantagePoint attrs) = case msg of
    InvestigatorPlayEvent iid eid _ (vantagePointLocation -> lid) _
      | eid == toId attrs -> do
        otherLocationsWithClues <-
          selectList $ LocationWithAnyClues <> NotLocation (LocationWithId lid)
        pushAll
          $ createCardEffect
              Cards.vantagePoint
              Nothing
              (toSource attrs)
              (LocationTarget lid)
          : [ chooseOne iid
              $ Label "Do not move a clue" []
              : [ targetLabel
                    lid'
                    [ RemoveClues (LocationTarget lid') 1
                    , PlaceClues (LocationTarget lid) 1
                    ]
                | lid' <- otherLocationsWithClues
                ]
            | notNull otherLocationsWithClues
            ]
          <> [discard attrs]
        pure e
    _ -> VantagePoint <$> runMessage msg attrs

newtype VantagePointEffect = VantagePointEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

vantagePointEffect :: EffectArgs -> VantagePointEffect
vantagePointEffect = cardEffect VantagePointEffect Cards.vantagePoint

instance HasModifiersFor VantagePointEffect where
  getModifiersFor target (VantagePointEffect a) | effectTarget a == target =
    pure $ toModifiers a [ShroudModifier (-1)]
  getModifiersFor _ _ = pure []

instance RunMessage VantagePointEffect where
  runMessage msg e@(VantagePointEffect attrs) = case msg of
    EndTurn _ -> do
      push (DisableEffect $ toId attrs)
      pure e
    _ -> VantagePointEffect <$> runMessage msg attrs
