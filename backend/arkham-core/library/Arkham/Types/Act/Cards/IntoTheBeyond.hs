module Arkham.Types.Act.Cards.IntoTheBeyond
  ( IntoTheBeyond(..)
  , intoTheBeyond
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Target

newtype IntoTheBeyond = IntoTheBeyond ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor env)

intoTheBeyond :: ActCard IntoTheBeyond
intoTheBeyond = act (2, A) IntoTheBeyond Cards.intoTheBeyond Nothing

instance HasActions IntoTheBeyond where
  getActions (IntoTheBeyond x) =
    mkAbility x 1 (ActionAbility Nothing $ ActionCost 1) : getActions x

instance (HasName env LocationId, ActRunner env) => RunMessage env IntoTheBeyond where
  runMessage msg a@(IntoTheBeyond attrs@ActAttrs {..}) = case msg of
    WhenEnterLocation _ lid -> do
      name <- getName lid
      a <$ when
        (name == "The Edge of the Universe")
        (push $ AdvanceAct actId (toSource attrs))
    AdvanceAct aid _ | aid == actId && onSide B attrs ->
      a <$ push (NextAct actId "02318")
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
                [ RemoveFromEncounterDiscard location
                , InvestigatorDrewEncounterCard iid location
                ]
            | location <- locationCards
            ]
          , UnfocusCards
          ]
        )
    _ -> IntoTheBeyond <$> runMessage msg attrs
