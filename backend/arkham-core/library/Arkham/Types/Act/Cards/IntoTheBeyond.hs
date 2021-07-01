module Arkham.Types.Act.Cards.IntoTheBeyond
  ( IntoTheBeyond(..)
  , intoTheBeyond
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
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Window

newtype IntoTheBeyond = IntoTheBeyond ActAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor env)

intoTheBeyond :: IntoTheBeyond
intoTheBeyond =
  IntoTheBeyond $ baseAttrs "02317" "Into the Beyond" (Act 2 A) Nothing

instance ActionRunner env => HasActions env IntoTheBeyond where
  getActions iid NonFast (IntoTheBeyond x) = withBaseActions iid NonFast x $ do
    pure
      [ ActivateCardAbilityAction
          iid
          (mkAbility (toSource x) 1 (ActionAbility Nothing $ ActionCost 1))
      ]
  getActions iid window (IntoTheBeyond x) = getActions iid window x

instance (HasName env LocationId, ActRunner env) => RunMessage env IntoTheBeyond where
  runMessage msg a@(IntoTheBeyond attrs@ActAttrs {..}) = case msg of
    WhenEnterLocation _ lid -> do
      name <- getName lid
      a <$ when
        (name == "The Edge of the Universe")
        (unshiftMessage $ AdvanceAct actId (toSource attrs))
    AdvanceAct aid _ | aid == actId && onSide B attrs ->
      a <$ unshiftMessage (NextAct actId "02318")
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
    _ -> IntoTheBeyond <$> runMessage msg attrs
