module Arkham.Homebrew.AgesUnwound.Treacheries.Omnipresence (omnipresence) where

import Arkham.Ability
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Helpers (Deck (..))
import Arkham.Helpers.Scenario (getEncounterDeck, scenarioField)
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Types (Field (ScenarioDiscard))
import Arkham.Treachery.Import.Lifted

newtype Omnipresence = Omnipresence TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

omnipresence :: TreacheryCard Omnipresence
omnipresence = treachery Omnipresence Cards.omnipresence

{- | "[free]: Discard Omnipresence." / "Forced - After the agenda advances:
Discard Omnipresence and place 1 doom on the current agenda."

The free ability carries no printed restriction, so the general permission rule
governs it: a triggered ability on a scenario card in play may be used only by an
investigator at the same location
(@mcp/references/rules/glossary/triggered_abilities.md@). That is load-bearing
here, since Omnipresence only ever attaches to an /empty/ location.
-}
instance HasAbilities Omnipresence where
  getAbilities (Omnipresence a) =
    [ restricted a 1 OnSameLocation $ FastAbility Free
    , mkAbility a 2 $ forced $ AgendaAdvances #after AnyAgenda
    ]

instance RunMessage Omnipresence where
  runMessage msg t@(Omnipresence attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      candidates <-
        select $ EmptyLocation <> not_ (LocationWithTreachery $ treacheryIs Cards.omnipresence)
      if null candidates
        then gainSurge attrs
        else do
          chooseTargetM iid candidates $ attachTreachery attrs
          doStep 1 msg
      pure t
    -- "Search the encounter deck and discard pile for each copy of Omnipresence
    -- and draw them." Every copy is obtained before any of them is drawn, so the
    -- copy drawn first cannot find -- and draw again -- the ones behind it.
    DoStep 1 (Revelation iid (isSource attrs -> True)) -> do
      inDeck <- filter isOmnipresence . unDeck <$> getEncounterDeck
      inDiscard <- filter isOmnipresence <$> scenarioField ScenarioDiscard
      let copies = inDeck <> inDiscard
      for_ copies obtainCard
      for_ copies $ push . InvestigatorDrewEncounterCard iid
      shuffleDeck Deck.EncounterDeck
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      toDiscard (attrs.ability 2) attrs
      placeDoomOnAgenda 1
      pure t
    _ -> Omnipresence <$> liftRunMessage msg attrs
   where
    isOmnipresence :: EncounterCard -> Bool
    isOmnipresence = (`cardMatch` cardIs Cards.omnipresence)
