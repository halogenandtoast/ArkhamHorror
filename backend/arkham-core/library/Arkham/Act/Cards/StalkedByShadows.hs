module Arkham.Act.Cards.StalkedByShadows
  ( StalkedByShadows(..)
  , stalkedByShadows
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Types
import qualified Arkham.Act.Cards as Cards
import Arkham.Act.Runner
import Arkham.Act.Helpers
import Arkham.Classes
import Arkham.CampaignLogKey
import Arkham.Criteria
import Arkham.Resolution
import Arkham.GameValue
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Message
import Arkham.Scenarios.APhantomOfTruth.Helpers

newtype StalkedByShadows = StalkedByShadows ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stalkedByShadows :: ActCard StalkedByShadows
stalkedByShadows = act (2, A) StalkedByShadows Cards.stalkedByShadows Nothing

instance HasAbilities StalkedByShadows where
  getAbilities (StalkedByShadows a) =
    [ restrictedAbility a 1 NoRestriction
      $ FastAbility (GroupClueCost (Static 1) Anywhere)
    ]

instance RunMessage StalkedByShadows where
  runMessage msg a@(StalkedByShadows attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      theOrganist <- getTheOrganist
      currentAgenda <- AgendaTarget <$> selectJust AnyAgenda
      push $ chooseOne iid [Label "Place 1 doom on the current agenda" [PlaceDoom currentAgenda 1], Label "Automatically evade The Organist" [EnemyEvaded iid theOrganist]]
      pure a
    AdvanceAct aid _ _ | aid == toId a && onSide B attrs -> do
      intrudedOnASecretMeeting <- getHasRecord YouIntrudedOnASecretMeeting
      a <$ push (ScenarioResolution $ Resolution $ if intrudedOnASecretMeeting then 2 else 1)
    _ -> StalkedByShadows <$> runMessage msg attrs
