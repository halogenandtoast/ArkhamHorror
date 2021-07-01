module Arkham.Types.Act.Cards.FindingANewWay
  ( FindingANewWay(..)
  , findingANewWay
  ) where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Game.Helpers
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Resolution
import Arkham.Types.Target
import Arkham.Types.Window

newtype FindingANewWay = FindingANewWay ActAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor env)

findingANewWay :: FindingANewWay
findingANewWay =
  FindingANewWay $ baseAttrs "02319" "Finding a New Way" (Act 4 A) Nothing

instance ActionRunner env => HasActions env FindingANewWay where
  getActions iid NonFast (FindingANewWay x) =
    withBaseActions iid NonFast x $ do
      pure
        [ ActivateCardAbilityAction
            iid
            (mkAbility (toSource x) 1 (ActionAbility Nothing $ ActionCost 1))
        ]
  getActions iid window (FindingANewWay x) = getActions iid window x

instance ActRunner env => RunMessage env FindingANewWay where
  runMessage msg a@(FindingANewWay attrs@ActAttrs {..}) = case msg of
    InvestigatorResigned _ -> do
      investigatorIds <- getSet @InScenarioInvestigatorId ()
      a <$ when
        (null investigatorIds)
        (unshiftMessage $ AdvanceAct actId (toSource attrs))
    AdvanceAct aid _ | aid == actId && onSide B attrs -> do
      a <$ unshiftMessage (ScenarioResolution $ Resolution 1)
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
    _ -> FindingANewWay <$> runMessage msg attrs
