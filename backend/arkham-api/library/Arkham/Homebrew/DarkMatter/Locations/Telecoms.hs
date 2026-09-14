module Arkham.Homebrew.DarkMatter.Locations.Telecoms (telecoms) where

import Arkham.Ability
import Arkham.CampaignLogKey (toCampaignLogKey)
import Arkham.GameValue
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (crossOffMemories, drawEvidence, getMemories)
import Arkham.Homebrew.DarkMatter.Key (DarkMatterKey (Memories))
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype Telecoms = Telecoms LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

telecoms :: LocationCard Telecoms
telecoms = location Telecoms Cards.telecoms 2 (PerPlayer 2)

{- | "[action] If it is currently act 2, cross out 1[per_investigator] of your
'Memories', as a group: Draw the top card of the 'Evidence' deck and read it.
(Group limit once per game.)"

The Memories toll is a group payment with no 'Cost' equivalent, so it is paid one
tally at a time. Per the user's ruling (2026-08-24): only the triggering
investigator must be here; ANY investigator's Memories may pay, eliminated ones
included, and the gate is at least 1 Memory across the group (not full
affordability). Contrast Cyclopean Caverns (Fragment of Carcosa), whose printed
text restricts payers to investigators at the location.
-}
instance HasAbilities Telecoms where
  getAbilities (Telecoms a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted
        a
        1
        ( Here
            <> ActExists (ActWithStep 2)
            <> exists (IncludeEliminated $ InvestigatorWithRecordCount (toCampaignLogKey Memories) (atLeast 1))
        )
        actionAbility

instance RunMessage Telecoms where
  runMessage msg l@(Telecoms attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      n <- perPlayer 1
      doStep n msg
      pure l
    DoStep n msg'@(UseThisAbility iid (isSource attrs -> True) 1) | n > 0 -> do
      payers <- filterM (fmap (> 0) . getMemories) =<< select (IncludeEliminated Anyone)
      if null payers
        then doStep 0 msg'
        else chooseOrRunOneM iid $ targets payers \payer -> do
          crossOffMemories payer 1
          doStep (n - 1) msg'
      pure l
    DoStep 0 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      drawEvidence iid
      pure l
    _ -> Telecoms <$> liftRunMessage msg attrs
