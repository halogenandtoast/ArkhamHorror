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
  getAbilities (IntoTheDarkness attrs) | onSide A attrs = do
    [ mkAbility attrs 1
        $ Objective
        $ ForcedAbility
        $ Enters Timing.After Anyone
        $ LocationWithTitle "Ritual Site"
      ]
  getAbilities _ = []

instance RunMessage IntoTheDarkness where
  runMessage msg a@(IntoTheDarkness attrs@ActAttrs {..}) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      push $ AdvanceAct actId (toSource attrs) AdvancedWithOther
      pure a
    AdvanceAct aid _ _ | aid == actId && onSide B attrs -> do
      playerCount <- getPlayerCount
      lead <- getLead
      pushAll
        $ [ ShuffleEncounterDiscardBackIn
          , DiscardUntilFirst lead (ActSource actId) Deck.EncounterDeck
              $ BasicCardMatch (CardWithType EnemyType)
          ]
          <> [ DiscardUntilFirst lead (ActSource actId) Deck.EncounterDeck
              $ BasicCardMatch (CardWithType EnemyType)
             | playerCount > 3
             ]
          <> [advanceActDeck attrs]
      pure a
    RequestedEncounterCard (ActSource aid) _ mcard | aid == actId -> do
      for_ mcard \card -> do
        ritualSite <- getJustLocationByName "Ritual Site"
        push $ SpawnEnemyAt (EncounterCard card) ritualSite
      pure a
    _ -> IntoTheDarkness <$> runMessage msg attrs
