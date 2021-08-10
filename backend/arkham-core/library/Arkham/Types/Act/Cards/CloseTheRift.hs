module Arkham.Types.Act.Cards.CloseTheRift
  ( CloseTheRift(..)
  , closeTheRift
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import qualified Arkham.Location.Cards as Locations
import Arkham.Types.Ability
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Target

newtype CloseTheRift = CloseTheRift ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor env)

closeTheRift :: ActCard CloseTheRift
closeTheRift = act
  (3, A)
  CloseTheRift
  Cards.closeTheRift
  (Just $ GroupClueCost
    (PerPlayer 3)
    (Just $ LocationWithTitle "The Edge of the Universe")
  )

instance HasActions CloseTheRift where
  getActions (CloseTheRift x) =
    mkAbility x 1 (ActionAbility Nothing $ ActionCost 1) : getActions x

instance ActRunner env => RunMessage env CloseTheRift where
  runMessage msg a@(CloseTheRift attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ | aid == actId && onSide B attrs -> do
      theEdgeOfTheUniverseId <- getJustLocationIdByName
        "The Edge of the Universe"
      tearThroughTimeId <- getRandom
      a <$ pushAll
        (resolve (RemoveLocation theEdgeOfTheUniverseId)
        <> [ PlaceLocation tearThroughTimeId Locations.tearThroughTime
           , NextAct actId "02319"
           ]
        )
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ push (DiscardTopOfEncounterDeck iid 3 (Just $ toTarget attrs))
    DiscardedTopOfEncounterDeck iid cards target | isTarget attrs target -> do
      let locationCards = filterLocations cards
      a <$ unless
        (null locationCards)
        (pushAll
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
