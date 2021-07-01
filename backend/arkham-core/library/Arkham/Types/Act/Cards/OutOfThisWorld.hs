module Arkham.Types.Act.Cards.OutOfThisWorld
  ( OutOfThisWorld(..)
  , outOfThisWorld
  )
where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Window

newtype OutOfThisWorld = OutOfThisWorld ActAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor env)

outOfThisWorld :: OutOfThisWorld
outOfThisWorld = OutOfThisWorld $ baseAttrs
  "02316"
  "Out of this World"
  (Act 1 A)
  (Just $ RequiredClues (PerPlayer 2) Nothing)

instance ActionRunner env => HasActions env OutOfThisWorld where
  getActions iid NonFast (OutOfThisWorld x) =
    withBaseActions iid NonFast x $ do
      pure
        [ ActivateCardAbilityAction
            iid
            (mkAbility (toSource x) 1 (ActionAbility Nothing $ ActionCost 1))
        ]
  getActions iid window (OutOfThisWorld x) = getActions iid window x

instance ActRunner env => RunMessage env OutOfThisWorld where
  runMessage msg a@(OutOfThisWorld attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ | aid == actId && onSide B attrs -> do
      theEdgeOfTheUniverseId <- getRandom
      a <$ unshiftMessages
        [PlaceLocation "02321" theEdgeOfTheUniverseId, NextAct actId "02317"]
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ unshiftMessage
        (DiscardTopOfEncounterDeck iid 3 (Just $ toTarget attrs))
    DiscardedTopOfEncounterDeck iid cards target | isTarget attrs target -> do
      let locationCards = filterLocations cards
      a <$ unless
        (null locationCards)
        (unshiftMessages
          [ FocusCards (map EncounterCard locationCards)
          , chooseOne
            iid
            [ TargetLabel
                (CardIdTarget $ location ^. cardIdL)
                [ RemoveFromEncounterDiscard location
                , InvestigatorDrewEncounterCard iid location
                ]
            | location <- locationCards
            ]
          , UnfocusCards
          ]
        )
    _ -> OutOfThisWorld <$> runMessage msg attrs
