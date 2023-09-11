module Arkham.Act.Cards.IntoTheDarkness where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Helpers
import Arkham.Act.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing

newtype IntoTheDarkness = IntoTheDarkness ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

intoTheDarkness :: ActCard IntoTheDarkness
intoTheDarkness = act (2, A) IntoTheDarkness Cards.intoTheDarkness Nothing

instance HasAbilities IntoTheDarkness where
  getAbilities (IntoTheDarkness attrs)
    | onSide A attrs =
        [ mkAbility attrs 1 $
            Objective $
              ForcedAbility $
                Enters Timing.After Anyone $
                  LocationWithTitle "Ritual Site"
        ]
  getAbilities _ = []

instance RunMessage IntoTheDarkness where
  runMessage msg a@(IntoTheDarkness attrs@ActAttrs {..}) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      a <$ push (AdvanceAct actId source AdvancedWithOther)
    AdvanceAct aid _ _ | aid == actId && onSide B attrs -> do
      playerCount <- getPlayerCount
      lead <- getLead
      if playerCount > 3
        then
          pushAll
            [ ShuffleEncounterDiscardBackIn
            , DiscardUntilFirst
                lead
                (ActSource actId)
                Deck.EncounterDeck
                (BasicCardMatch $ CardWithType EnemyType)
            , DiscardUntilFirst
                lead
                (ActSource actId)
                Deck.EncounterDeck
                (BasicCardMatch $ CardWithType EnemyType)
            , advanceActDeck attrs
            ]
        else
          pushAll
            [ ShuffleEncounterDiscardBackIn
            , DiscardUntilFirst
                lead
                (ActSource actId)
                Deck.EncounterDeck
                (BasicCardMatch $ CardWithType EnemyType)
            , advanceActDeck attrs
            ]
      pure a
    RequestedEncounterCard (ActSource aid) _ mcard | aid == actId -> case mcard of
      Nothing -> pure a
      Just card -> do
        ritualSiteId <- getJustLocationByName "Ritual Site"
        a <$ pushAll [SpawnEnemyAt (EncounterCard card) ritualSiteId]
    _ -> IntoTheDarkness <$> runMessage msg attrs
