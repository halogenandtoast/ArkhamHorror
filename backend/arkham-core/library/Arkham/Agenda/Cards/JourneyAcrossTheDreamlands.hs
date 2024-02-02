module Arkham.Agenda.Cards.JourneyAcrossTheDreamlands (JourneyAcrossTheDreamlands (..), journeyAcrossTheDreamlands) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Trait (Trait (Port))

newtype JourneyAcrossTheDreamlands = JourneyAcrossTheDreamlands AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

journeyAcrossTheDreamlands :: AgendaCard JourneyAcrossTheDreamlands
journeyAcrossTheDreamlands = agenda (1, A) JourneyAcrossTheDreamlands Cards.journeyAcrossTheDreamlands (Static 7)

instance HasAbilities JourneyAcrossTheDreamlands where
  getAbilities (JourneyAcrossTheDreamlands x) =
    [ withTooltip
        "_Resign_. Venturing into the unknown has become too dangerous, so you return to safety with the information you've gathered."
        $ restrictedAbility x 1 (exists $ You <> at_ (withTrait Port))
        $ ActionAbility [Action.Resign]
        $ ActionCost 1
    ]

instance RunMessage JourneyAcrossTheDreamlands where
  runMessage msg a@(JourneyAcrossTheDreamlands attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      corsairs <- getSetAsideCardsMatching (cardIs Enemies.corsairOfLeng)
      pushAll
        [ ShuffleCardsIntoDeck Deck.EncounterDeck corsairs
        , ShuffleEncounterDiscardBackIn
        , advanceAgendaDeck attrs
        ]
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ Resign iid
      pure a
    _ -> JourneyAcrossTheDreamlands <$> runMessage msg attrs
