module Arkham.Act.Cards.IntoTheDarkness where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Types
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Helpers
import Arkham.Act.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.Matcher
import Arkham.Message
import Arkham.Source
import Arkham.Timing qualified as Timing

newtype IntoTheDarkness = IntoTheDarkness ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

intoTheDarkness :: ActCard IntoTheDarkness
intoTheDarkness = act (2, A) IntoTheDarkness Cards.intoTheDarkness Nothing

instance HasAbilities IntoTheDarkness where
  getAbilities (IntoTheDarkness attrs) | onSide A attrs =
    [ mkAbility attrs 1
        $ Objective
        $ ForcedAbility
        $ Enters Timing.After Anyone
        $ LocationWithTitle "Ritual Site"
    ]
  getAbilities _ = []

instance RunMessage IntoTheDarkness where
  runMessage msg a@(IntoTheDarkness attrs@ActAttrs {..}) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      a <$ push (AdvanceAct actId source AdvancedWithOther)
    AdvanceAct aid _ _ | aid == actId && onSide B attrs -> do
      playerCount <- getPlayerCount
      if playerCount > 3
        then a <$ pushAll
          [ ShuffleEncounterDiscardBackIn
          , DiscardEncounterUntilFirst
            (ActSource actId)
            Nothing
            (CardWithType EnemyType)
          , DiscardEncounterUntilFirst
            (ActSource actId)
            Nothing
            (CardWithType EnemyType)
          , AdvanceActDeck actDeckId (toSource attrs)
          ]
        else a <$ pushAll
          [ ShuffleEncounterDiscardBackIn
          , DiscardEncounterUntilFirst
            (ActSource actId)
            Nothing
            (CardWithType EnemyType)
          , AdvanceActDeck actDeckId (toSource attrs)
          ]
    RequestedEncounterCard (ActSource aid) _ mcard | aid == actId -> case mcard of
      Nothing -> pure a
      Just card -> do
        ritualSiteId <- getJustLocationIdByName "Ritual Site"
        a <$ pushAll [SpawnEnemyAt (EncounterCard card) ritualSiteId]
    _ -> IntoTheDarkness <$> runMessage msg attrs
