module Arkham.Agenda.Cards.LoathsomeParasites (loathsomeParasites) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Card (cardMatch, card_)
import Arkham.Enemy.Creation (EnemyCreationMethod (..))
import Arkham.Helpers (Deck (..))
import Arkham.Helpers.Scenario (getEncounterDeck)
import Arkham.Matcher

newtype LoathsomeParasites = LoathsomeParasites AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

loathsomeParasites :: AgendaCard LoathsomeParasites
loathsomeParasites = agenda (2, A) LoathsomeParasites Cards.loathsomeParasites (Static 6)

instance HasAbilities LoathsomeParasites where
  getAbilities (LoathsomeParasites a) =
    [ restricted a 1 (notExists NonEliteEnemy)
        $ forced
        $ PlacedDoomCounter #after AnySource (targetIs a)
    ]

instance RunMessage LoathsomeParasites where
  runMessage msg a@(LoathsomeParasites attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      deck <- unDeck <$> getEncounterDeck
      (others, rest) <- breakM (pure . (`cardMatch` card_ #enemy)) deck
      case rest of
        -- No enemy in the deck; leave the deck as revealed (nothing to spawn).
        -- TODO: if the encounter deck is exhausted without an enemy, the discard
        -- should be shuffled in and revealing continued.
        [] -> pure ()
        (enemyCard : remaining) -> do
          -- Shuffle each other revealed card and place them on top of the deck.
          shuffledOthers <- shuffleM others
          setEncounterDeck $ Deck (shuffledOthers <> remaining)
          createEnemyWith_ enemyCard (SpawnAtLocationMatching $ FirstLocation [EmptyLocation, Anywhere]) id
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      selectEach (not_ ResignedInvestigator <> UneliminatedInvestigator) \iid ->
        investigatorDefeated attrs iid
      pure a
    _ -> LoathsomeParasites <$> liftRunMessage msg attrs
