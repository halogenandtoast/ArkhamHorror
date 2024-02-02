module Arkham.Act.Cards.ExploringTheRainforest (
  ExploringTheRainforest (..),
  exploringTheRainforest,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Matcher
import Arkham.Timing qualified as Timing
import Arkham.Trait

newtype ExploringTheRainforest = ExploringTheRainforest ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

exploringTheRainforest :: ActCard ExploringTheRainforest
exploringTheRainforest =
  act (1, A) ExploringTheRainforest Cards.exploringTheRainforest Nothing

instance HasAbilities ExploringTheRainforest where
  getAbilities (ExploringTheRainforest x) =
    [ mkAbility x 1
        $ Objective
        $ ReactionAbility (RoundEnds Timing.When)
        $ GroupClueCost (PerPlayer 3) (NotLocation $ LocationWithTrait Campsite)
    ]

instance RunMessage ExploringTheRainforest where
  runMessage msg a@(ExploringTheRainforest attrs) = case msg of
    UseCardAbility iid source 1 _ _
      | isSource attrs source ->
          a <$ push (AdvanceAct (toId a) (InvestigatorSource iid) AdvancedWithClues)
    AdvanceAct aid _ _ | aid == actId attrs && onSide B attrs -> do
      ichtaca <-
        fromJustNote "Ichtaca was not set aside"
          . listToMaybe
          <$> getSetAsideCardsMatching (CardWithTitle "Ichtaca")
      locationId <- selectJust LeadInvestigatorLocation
      createIchtaca <- createEnemyAt_ ichtaca locationId Nothing
      pushAll
        [ createIchtaca
        , ShuffleEncounterDiscardBackIn
        , AdvanceActDeck (actDeckId attrs) (toSource attrs)
        ]
      pure a
    _ -> ExploringTheRainforest <$> runMessage msg attrs
