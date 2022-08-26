module Arkham.Agenda.Cards.SomethingStirs
  ( SomethingStirs(..)
  , somethingStirs
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Helpers.Query
import Arkham.Location.Types (Field(..))
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing

newtype SomethingStirs = SomethingStirs AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

somethingStirs :: AgendaCard SomethingStirs
somethingStirs =
  agendaWith
      (1, A)
      SomethingStirs
      Cards.somethingStirs
      (StaticWithPerPlayer 6 1)
    $ removeDoomMatchersL
    %~ (\m -> m { removeDoomLocations = Nowhere })

instance HasAbilities SomethingStirs where
  getAbilities (SomethingStirs a) =
    [ mkAbility a 1
        $ ForcedAbility
        $ AgendaAdvances Timing.When
        $ AgendaWithId
        $ toId a
    ]

-- ability does not do anything, just triggers the button
instance RunMessage SomethingStirs where
  runMessage msg a@(SomethingStirs attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      pure a
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      maxDoom <- getMax0 <$> selectAgg Max LocationDoom Anywhere
      targets <- selectList $ LocationWithDoom $ EqualTo (Static maxDoom)
      harbingerOfValusia <- getSetAsideCard Enemies.harbingerOfValusia
      leadInvestigatorId <- getLeadInvestigatorId
      pushAll
        $ chooseOne
            leadInvestigatorId
            [ targetLabel
                target
                [CreateEnemyAt harbingerOfValusia target Nothing]
            | target <- targets
            ]
        : [AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)]
      pure a
    _ -> SomethingStirs <$> runMessage msg attrs
