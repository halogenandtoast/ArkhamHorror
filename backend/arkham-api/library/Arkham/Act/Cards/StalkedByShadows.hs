module Arkham.Act.Cards.StalkedByShadows (
  StalkedByShadows (..),
  stalkedByShadows,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Helpers
import Arkham.Act.Runner
import Arkham.CampaignLogKey
import Arkham.Classes
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Message (Message (EnemyEvaded))
import Arkham.Scenarios.APhantomOfTruth.Helpers

newtype StalkedByShadows = StalkedByShadows ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stalkedByShadows :: ActCard StalkedByShadows
stalkedByShadows = act (2, A) StalkedByShadows Cards.stalkedByShadows Nothing

instance HasAbilities StalkedByShadows where
  getAbilities (StalkedByShadows a) =
    [ groupLimit PerRound
        $ restrictedAbility a 1 NoRestriction
        $ FastAbility (GroupClueCost (PerPlayer 1) Anywhere)
    ]

instance RunMessage StalkedByShadows where
  runMessage msg a@(StalkedByShadows attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      theOrganist <- getTheOrganist
      currentAgenda <- AgendaTarget <$> selectJust AnyAgenda
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ Label "Place 1 doom on the current agenda" [PlaceDoom (toAbilitySource attrs 1) currentAgenda 1]
          , Label "Automatically evade The Organist" [EnemyEvaded iid theOrganist]
          ]
      pure a
    AdvanceAct aid _ _ | aid == toId a && onSide B attrs -> do
      intrudedOnASecretMeeting <- getHasRecord YouIntrudedOnASecretMeeting
      push (scenarioResolution $ if intrudedOnASecretMeeting then 2 else 1)
      pure a
    _ -> StalkedByShadows <$> runMessage msg attrs
