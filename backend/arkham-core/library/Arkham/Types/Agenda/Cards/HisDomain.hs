module Arkham.Types.Agenda.Cards.HisDomain
  ( HisDomain
  , hisDomain
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Matcher hiding (InvestigatorDefeated, PlaceUnderneath)
import Arkham.Types.Matcher qualified as Matcher
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Timing qualified as Timing
import Arkham.Types.Window (Window(..))
import Arkham.Types.Window qualified as Window

newtype HisDomain = HisDomain AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hisDomain :: AgendaCard HisDomain
hisDomain = agenda (3, A) HisDomain Cards.hisDomain (Static 8)

instance HasAbilities HisDomain where
  getAbilities (HisDomain attrs) =
    [ mkAbility attrs 1 $ ForcedAbility $ Matcher.PlaceUnderneath
        Timing.When
        (TargetIs ActDeckTarget)
        (CardWithType EnemyType)
    ]

instance AgendaRunner env => RunMessage env HisDomain where
  runMessage msg a@(HisDomain attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      investigatorIds <- selectList UneliminatedInvestigator
      -- TODO: If any investigators resigned, those investigators may immediately advance to act 4b.
      a <$ pushAll (map (InvestigatorDefeated (toSource attrs)) investigatorIds)
    UseCardAbility _ source [Window _ (Window.PlaceUnderneath _ card)] 1 _
      | isSource attrs source -> do
        let ec = fromJustNote "wrong card type" $ preview _EncounterCard card
        removeAllMessagesMatching \case
          PlacedUnderneath ActDeckTarget card' -> card == card'
          CheckWindow _ [Window _ (Window.PlaceUnderneath ActDeckTarget card')]
            -> card == card'
          _ -> False
        a <$ push (ShuffleIntoEncounterDeck [ec])
    _ -> HisDomain <$> runMessage msg attrs
