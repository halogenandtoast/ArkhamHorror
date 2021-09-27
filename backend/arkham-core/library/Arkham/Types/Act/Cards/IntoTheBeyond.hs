module Arkham.Types.Act.Cards.IntoTheBeyond
  ( IntoTheBeyond(..)
  , intoTheBeyond
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Game.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Timing qualified as Timing

newtype IntoTheBeyond = IntoTheBeyond ActAttrs
  deriving anyclass (IsAct, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

intoTheBeyond :: ActCard IntoTheBeyond
intoTheBeyond = act (2, A) IntoTheBeyond Cards.intoTheBeyond Nothing

instance HasAbilities IntoTheBeyond where
  getAbilities (IntoTheBeyond x) = withBaseAbilities
    x
    [ mkAbility x 1 $ ActionAbility Nothing $ ActionCost 1
    , mkAbility x 2
    $ Objective
    $ ForcedAbility
    $ Enters Timing.When Anyone
    $ LocationWithTitle "The Edge of the Universe"
    ]

instance ActRunner env => RunMessage env IntoTheBeyond where
  runMessage msg a@(IntoTheBeyond attrs@ActAttrs {..}) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ push (DiscardTopOfEncounterDeck iid 3 (Just $ toTarget attrs))
    UseCardAbility _ source _ 2 _ | isSource attrs source ->
      a <$ push (AdvanceAct (toId attrs) source)
    AdvanceAct aid _ | aid == actId && onSide B attrs ->
      a <$ push (NextAct actId "02318")
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
                [ RemoveFromEncounterDiscard location
                , InvestigatorDrewEncounterCard iid location
                ]
            | location <- locationCards
            ]
          , UnfocusCards
          ]
        )
    _ -> IntoTheBeyond <$> runMessage msg attrs
