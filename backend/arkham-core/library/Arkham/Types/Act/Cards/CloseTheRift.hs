module Arkham.Types.Act.Cards.CloseTheRift
  ( CloseTheRift(..)
  , closeTheRift
  ) where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.LocationMatcher
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Window

newtype CloseTheRift = CloseTheRift ActAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor env)

closeTheRift :: CloseTheRift
closeTheRift = CloseTheRift $ baseAttrs
  "02318"
  "Close the Rift"
  (Act 3 A)
  (Just $ RequiredClues
    (PerPlayer 3)
    (Just $ LocationWithTitle "The Edge of the Universe")
  )

instance ActionRunner env => HasActions env CloseTheRift where
  getActions iid NonFast (CloseTheRift x) = withBaseActions iid NonFast x $ do
    pure
      [ ActivateCardAbilityAction
          iid
          (mkAbility (toSource x) 1 (ActionAbility Nothing $ ActionCost 1))
      ]
  getActions iid window (CloseTheRift x) = getActions iid window x

instance ActRunner env => RunMessage env CloseTheRift where
  runMessage msg a@(CloseTheRift attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ | aid == actId && onSide B attrs -> do
      theEdgeOfTheUniverseId <- getJustLocationIdByName
        "The Edge of the Universe"
      tearThroughTimeId <- getRandom
      a <$ unshiftMessages
        (resolve (RemoveLocation theEdgeOfTheUniverseId)
        <> [PlaceLocation "02322" tearThroughTimeId, NextAct actId "02319"]
        )
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
                (CardIdTarget $ toCardId location)
                [InvestigatorDrewEncounterCard iid location]
            | location <- locationCards
            ]
          ]
        )
    _ -> CloseTheRift <$> runMessage msg attrs
