module Arkham.Act.Cards.FindingANewWay (
  FindingANewWay (..),
  findingANewWay,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.Matcher

newtype FindingANewWay = FindingANewWay ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

findingANewWay :: ActCard FindingANewWay
findingANewWay = act (4, A) FindingANewWay Cards.findingANewWay Nothing

instance HasAbilities FindingANewWay where
  getAbilities (FindingANewWay x) | onSide A x = do
    [ mkAbility x 1 $ ActionAbility [] $ ActionCost 1
      , restrictedAbility x 2 AllUndefeatedInvestigatorsResigned
          $ Objective
          $ ForcedAbility AnyWindow
      ]
  getAbilities _ = []

instance RunMessage FindingANewWay where
  runMessage msg a@(FindingANewWay attrs@ActAttrs {..}) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push
        $ DiscardTopOfEncounterDeck
          iid
          3
          (toSource attrs)
          (Just $ toTarget attrs)
      pure a
    UseCardAbility _ source 2 _ _ | isSource attrs source -> do
      a <$ push (AdvanceAct (toId attrs) source AdvancedWithOther)
    AdvanceAct aid _ _ | aid == actId && onSide B attrs -> do
      a <$ push (scenarioResolution 1)
    DiscardedTopOfEncounterDeck iid cards _ target | isTarget attrs target -> do
      let locationCards = filterLocations cards
      player <- getPlayer iid
      unless (null locationCards)
        $ pushAll
          [ FocusCards (map EncounterCard locationCards)
          , chooseOne
              player
              [ targetLabel
                (toCardId location)
                [InvestigatorDrewEncounterCard iid location]
              | location <- locationCards
              ]
          ]
      pure a
    _ -> FindingANewWay <$> runMessage msg attrs
