module Arkham.Types.Act.Cards.CloseTheRift
  ( CloseTheRift(..)
  , closeTheRift
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
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
  deriving anyclass (IsAct, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

closeTheRift :: ActCard CloseTheRift
closeTheRift = act
  (3, A)
  CloseTheRift
  Cards.closeTheRift
  (Just $ GroupClueCost
    (PerPlayer 3)
    (LocationWithTitle "The Edge of the Universe")
  )

instance HasAbilities CloseTheRift where
  getAbilities (CloseTheRift x) =
    withBaseAbilities x [mkAbility x 1 $ ActionAbility Nothing $ ActionCost 1]

instance ActRunner env => RunMessage env CloseTheRift where
  runMessage msg a@(CloseTheRift attrs@ActAttrs {..}) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ push (DiscardTopOfEncounterDeck iid 3 (Just $ toTarget attrs))
    AdvanceAct aid _ | aid == actId && onSide B attrs -> do
      theEdgeOfTheUniverseId <- getJustLocationIdByName
        "The Edge of the Universe"
      tearThroughTime <- getSetAsideCard Locations.tearThroughTime
      a <$ pushAll
        (resolve (RemoveLocation theEdgeOfTheUniverseId)
        <> [ PlaceLocation tearThroughTime
           , AdvanceActDeck actDeckId (toSource attrs)
           ]
        )
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
