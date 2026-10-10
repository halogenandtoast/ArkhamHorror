module Arkham.Homebrew.AgesUnwound.Acts.Preservation (preservation) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Agenda (getCurrentAgendaStep)
import Arkham.Helpers.Investigator (getJustLocation)
import Arkham.Homebrew.AgesUnwound.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.AgesUnwound.CardDefs.Agendas qualified as Agendas
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgesUnwound.Scenarios.TimeRunsOut.Helpers
import Arkham.Investigator.Types (Field (InvestigatorClues))
import Arkham.Matcher
import Arkham.Projection

newtype Preservation = Preservation ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

preservation :: ActCard Preservation
preservation = act (2, A) Preservation Cards.preservation Nothing

{- | "[action] Investigators at your location spend 2[per_investigator] clues, as
a group: Remove each doom from your location. Place 1 resource on your location.
(Group limit once per game at each location.) /
__Objective__ - At the end of the round, if all locations are warded, advance."

The per-location limit is read off the board rather than remembered: the ability's
whole effect is to put the location's ward on it, and nothing in the scenario
takes a resource back off a location, so "not yet warded" and "not yet used here"
are the same set. The engine's ability limits are per card or per player, neither
of which can be keyed by location.
-}
instance HasAbilities Preservation where
  getAbilities (Preservation a) =
    [ restricted a 1 (exists $ You <> at_ unwarded)
        $ actionAbilityWithCost (SameLocationGroupClueCost (PerPlayer 2) YourLocation)
    , restricted a 2 (notExists $ Anywhere <> unwarded) $ Objective $ forced $ RoundEnds #when
    ]

{- | Act 2b /A Familiar Face/: "Remove all doom from play. If it is agenda 1a,
advance to agenda 2a. /
Each investigator loses all of their clues. Remove all clues from each location.
Put the set-aside The Past, The Present and The Future locations into play. /
Put the set-aside Yourself enemy next to the agenda deck. Each investigator puts
the top card of their deck facedown in their threat area, as a copy of Yourself.
'You' on each copy of Yourself refers to the owner of the card."

The set-aside /Yourself/ card itself stays a card beside the agenda deck -- it is
the reference the copies are read from, not a playing piece -- while each copy is
a real enemy minted onto a player card by 'spawnYourself'.
-}
instance RunMessage Preservation where
  runMessage msg a@(Preservation attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      lid <- getJustLocation iid
      removeAllDoom (attrs.ability 1) lid
      wardLocation (attrs.ability 1) lid
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advanceVia #other attrs attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push $ RemoveAllDoomFromPlay defaultRemoveDoomMatchers
      whenM ((== 1) <$> getCurrentAgendaStep)
        $ advanceToAgendaA attrs Agendas.familiarAdversaries

      eachInvestigator \iid -> do
        clues <- field InvestigatorClues iid
        when (clues > 0) $ push $ InvestigatorSpendClues iid clues
      eachLocation (removeAllClues attrs)

      placeSetAsideLocations_ [Locations.thePast, Locations.thePresent, Locations.theFuture]

      yourselfCard <- getSetAsideCard Enemies.yourself
      push $ PlaceNextTo AgendaDeckTarget [yourselfCard]
      eachInvestigator spawnYourself

      advanceActDeck attrs
      pure a
    _ -> Preservation <$> liftRunMessage msg attrs
