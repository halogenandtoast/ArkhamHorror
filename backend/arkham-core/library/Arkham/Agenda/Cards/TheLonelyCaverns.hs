module Arkham.Agenda.Cards.TheLonelyCaverns
  ( TheLonelyCaverns(..)
  , theLonelyCaverns
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Helpers.Investigator
import Arkham.Helpers.Location
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing

newtype TheLonelyCaverns = TheLonelyCaverns AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theLonelyCaverns :: AgendaCard TheLonelyCaverns
theLonelyCaverns =
  agendaWith (1, A) TheLonelyCaverns Cards.theLonelyCaverns (Static 7)
    $ removeDoomMatchersL
    %~ (\m -> m { removeDoomLocations = Nowhere })

instance HasAbilities TheLonelyCaverns where
  getAbilities (TheLonelyCaverns a) =
    [ restrictedAbility
        a
        1
        (LocationExists $ YourLocation <> LocationWithoutClues)
      $ ActionAbility (Just Action.Explore)
      $ ActionCost 1
    , mkAbility a 2
      $ ForcedAbility
      $ AgendaAdvances Timing.When
      $ AgendaWithId
      $ toId a
    ]

instance RunMessage TheLonelyCaverns where
  runMessage msg a@(TheLonelyCaverns attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      locationSymbols <- toConnections =<< getJustLocation iid
      push $ Explore
        iid
        (toSource attrs)
        (CardWithOneOf $ map CardWithPrintedLocationSymbol locationSymbols)
      pure a
    UseCardAbility _ (isSource attrs -> True) 2 _ _ -> do
      pure a
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      harbingerAlive <- getHasRecord TheHarbingerIsStillAlive
      yigsFury <- getRecordCount YigsFury
      harbinger <- genCard Enemies.harbingerOfValusia
      locationId <- if yigsFury >= 8
        then getJustLocation =<< getLeadInvestigatorId
        else selectJust $ FarthestLocationFromAll Anywhere
      pushAll
        $ [ CreateEnemyAt harbinger locationId (Just $ toTarget attrs)
          | harbingerAlive
          ]
        <> [AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)]
      pure a
    CreatedEnemyAt harbingerId _ (isTarget attrs -> True) -> do
      startingDamage <- getRecordCount TheHarbingerIsStillAlive
      when (startingDamage > 0) $ push $ PlaceDamage
        (EnemyTarget harbingerId)
        startingDamage
      pure a
    _ -> TheLonelyCaverns <$> runMessage msg attrs
