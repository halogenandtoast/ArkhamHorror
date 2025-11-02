module Arkham.Agenda.Cards.TheChase (theChase) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.TheScarletKeys.Key.Cards qualified as ScarletKeys
import Arkham.Campaigns.TheScarletKeys.Key.Matcher
import Arkham.Deck qualified as Deck
import Arkham.Helpers.Query (inTurnOrder)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype TheChase = TheChase AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theChase :: AgendaCard TheChase
theChase = agenda (1, A) TheChase Cards.theChase (Static 3)

instance HasAbilities TheChase where
  getAbilities (TheChase a) =
    [ restricted
        a
        1
        ( exists
            $ scarletKeyIs ScarletKeys.theTwistedAntiprism
            <> ScarletKeyWithInvestigator (InvestigatorAt (locationIs Locations.galataDocks))
        )
        $ Objective
        $ FastAbility Free
    , restricted
        a
        2
        ( exists
            $ scarletKeyIs ScarletKeys.theTwistedAntiprism
            <> ScarletKeyWithEnemy (EnemyAt (locationIs Locations.galataDocks))
        )
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage TheChase where
  runMessage msg a@(TheChase attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      enemies <- select $ InPlayEnemy AnyEnemy
      leadChooseOneAtATimeM do
        targets enemies \x -> do
          push $ HunterMove x
          temporaryModifier x attrs DoNotExhaust do
            push $ ForTarget (toTarget x) EnemiesAttack

      enemyCount <- selectCount AnyEnemy
      investigators <- inTurnOrder =<< select UneliminatedInvestigator

      when (enemyCount < length investigators) do
        push $ ForInvestigators investigators msg
      revertAgenda attrs
      pure a
    ForInvestigators (iid : rest) msg'@(AdvanceAgenda (isSide B attrs -> True)) -> do
      enemyCount <- selectCount AnyEnemy
      investigatorCount <- selectCount UneliminatedInvestigator
      when (enemyCount < investigatorCount) do
        discardUntilFirst iid attrs Deck.EncounterDeck (basic $ #enemy <> #cultist)
        push $ ForInvestigators rest msg'
      pure a
    RequestedEncounterCard (isSource attrs -> True) (Just iid) (Just ec) -> do
      drawCard iid ec
      pure a
    RequestedEncounterCard (isSource attrs -> True) (Just iid) Nothing -> do
      shuffleEncounterDiscardBackIn
      discardUntilFirst iid attrs Deck.EncounterDeck (basic $ #enemy <> #cultist)
      pure a
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push R1
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      push R6
      pure a
    _ -> TheChase <$> liftRunMessage msg attrs
