module Arkham.Agenda.Cards.TheHierophantV (theHierophantV) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted hiding (EnemyDefeated)
import Arkham.Card
import Arkham.Helpers.Query
import Arkham.Helpers.Window (defeatedEnemy)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Cultist, SilverTwilight))

newtype TheHierophantV = TheHierophantV AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theHierophantV :: AgendaCard TheHierophantV
theHierophantV = agenda (1, A) TheHierophantV Cards.theHierophantV (Static 8)

instance HasAbilities TheHierophantV where
  getAbilities (TheHierophantV a) =
    [mkAbility a 1 $ forced $ EnemyDefeated #when You ByAny $ EnemyWithTrait SilverTwilight]

-- given a list of investigators and a list of cultists have each investigator choose a cultist to draw
buildDrawCultists
  :: ReverseQueue m => [Card] -> NonEmpty InvestigatorId -> NonEmpty EncounterCard -> m ()
buildDrawCultists focused (investigator :| []) cards = do
  focusCards focused do
    chooseOneM investigator $ targets (toList cards) $ drawCard investigator
buildDrawCultists focused (investigator :| (nextInvestigator : remainingInvestigators)) cards = do
  focusCards focused do
    chooseOneM investigator do
      for_ (eachWithRest $ toList cards) \(card, rest) -> do
        targeting card do
          drawCard investigator card
          for_ (nonEmpty rest)
            $ buildDrawCultists
              (deleteFirst (toCard card) focused)
              (nextInvestigator :| remainingInvestigators)

instance RunMessage TheHierophantV where
  runMessage msg a@(TheHierophantV attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      lead <- getLead
      discardTopOfEncounterDeckAndHandle lead attrs 5 attrs
      advanceAgendaDeck attrs
      pure a
    DiscardedTopOfEncounterDeck _ cards _ (isTarget attrs -> True) -> do
      void $ runMaybeT do
        cultists <- hoistMaybe $ nonEmpty $ filter (`cardMatch` (CardWithTrait Cultist <> #enemy)) cards
        investigators <- MaybeT $ nonEmpty <$> getInvestigators
        lift $ buildDrawCultists (map toCard cards) investigators cultists
      pure a
    UseCardAbility _ (isSource attrs -> True) 1 (defeatedEnemy -> enemy) _ -> do
      enemiesWithDoom <- select $ EnemyAt (locationWithEnemy enemy) <> EnemyWithAnyDoom
      for_ enemiesWithDoom \enemy' -> do
        removeDoom attrs enemy' 1
        placeDoomOnAgenda 1
      pure a
    _ -> TheHierophantV <$> liftRunMessage msg attrs
