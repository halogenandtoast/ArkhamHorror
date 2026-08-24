module Arkham.Homebrew.DarkMatter.Agendas.SignsFromAldebaran (signsFromAldebaran) where

import Arkham.Ability
import Arkham.Agenda.Import.Lifted
import Arkham.Card (toCard)
import Arkham.Deck qualified as Deck
import Arkham.Helpers.Query (getLead)
import Arkham.Helpers.Window (getDefeatedAsset)
import Arkham.Homebrew.DarkMatter.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.DarkMatter.Helpers (addImpendingDoom)
import Arkham.Homebrew.DarkMatter.Sets qualified as Set
import Arkham.Homebrew.DarkMatter.Traits (pattern Brain)
import Arkham.Matcher hiding (AssetDefeated)
import Arkham.Matcher qualified as Matcher
import Arkham.Trait (Trait (Byakhee))

newtype SignsFromAldebaran = SignsFromAldebaran AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

signsFromAldebaran :: AgendaCard SignsFromAldebaran
signsFromAldebaran = agenda (2, A) SignsFromAldebaran Cards.signsFromAldebaran (Static 8)

{- | "Forced - When a [[Brain]] story asset is defeated: Remove it from the game
and add 1 tally mark under 'Impending Doom' in your Campaign Log."
-}
instance HasAbilities SignsFromAldebaran where
  getAbilities (SignsFromAldebaran a) =
    [mkAbility a 1 $ forced $ Matcher.AssetDefeated #when ByAny (AssetWithTrait Brain)]

instance RunMessage SignsFromAldebaran where
  runMessage msg a@(SignsFromAldebaran attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (getDefeatedAsset -> aid) _ -> do
      push $ RemoveFromGame (AssetTarget aid)
      addImpendingDoom 1
      pure a
    {- Agenda 2b, "They Came from Above!":

    "Shuffle the set aside Interstellar Predators encounter set into the
    encounter deck, along with the encounter discard pile.
    Discard cards from the top of the encounter deck until a Byakhee enemy is
    discarded. Spawn it at the Entrance Tunnel."

    The set is shuffled in before the discard-until, so a Byakhee from it can be
    the one that is found. -}
    AdvanceAgenda (isSide B attrs -> True) -> do
      shuffleSetAsideEncounterSetIntoEncounterDeck Set.InterstellarPredators
      shuffleEncounterDiscardBackIn
      lead <- getLead
      discardUntilFirst lead attrs Deck.EncounterDeck $ basic (#enemy <> withTrait Byakhee)
      advanceAgendaDeck attrs
      pure a
    RequestedEncounterCard (isSource attrs -> True) _ (Just ec) -> do
      entranceTunnel <- selectJust $ locationIs Locations.entranceTunnel
      createEnemyAt_ (toCard ec) entranceTunnel
      pure a
    _ -> SignsFromAldebaran <$> liftRunMessage msg attrs
