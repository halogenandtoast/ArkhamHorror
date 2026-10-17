module Arkham.Homebrew.AgesUnwound.Acts.JustOneMan (justOneMan) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Card.CardType
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Homebrew.AgesUnwound.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgesUnwound.Scenarios.TheMyriadGentleman.Helpers
import Arkham.Matcher
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype JustOneMan = JustOneMan ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

justOneMan :: ActCard JustOneMan
justOneMan = act (2, A) JustOneMan Cards.justOneMan Nothing

{- | "Objective - At the end of the round, investigators at the Landing may spend
the requisite number of clues to advance."

"Forced - When a treachery card would search, reveal or discard from the
encounter deck to find an enemy: Instead spawn a copy of The Myriad Gentleman,
treating it as having been found by that treachery."

The trigger is about what the search is /for/, which lives in the matcher the
search was handed rather than in whatever it turns up -- it has to fire when the
deck holds no enemy at all. 'SearchingFor' reads that matcher; see
'Arkham.Matcher.Card.SearchMatcher'.
-}
instance HasAbilities JustOneMan where
  getAbilities (JustOneMan x) =
    [ mkAbility x 1
        $ Objective
        $ triggered (RoundEnds #when)
        $ GroupClueCost (PerPlayer 4) (locationIs Locations.landing)
    , mkAbility x 2
        $ forced
        $ WouldFindEncounterCard #when Anyone #treachery (SearchingFor EnemyType)
    ]

instance RunMessage JustOneMan where
  runMessage msg a@(JustOneMan attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advanceVia #clues attrs (attrs.ability 1)
      pure a
    {- "Instead spawn a copy of The Myriad Gentleman, treating it as having been
    found by that treachery." Cancelling the batch drops the search, and the copy
    spawns engaged with the searching investigator -- which is where the only card
    that triggers this, /Out of Phase/, puts what it finds, and it keeps the
    copy's bookkeeping in 'spawnMyriadCopiesEngagedWith' rather than handing a
    minted card back with no owner recorded. -}
    UseCardAbility _ (isSource attrs -> True) 2 ws _ -> do
      cancelBatch (Window.getBatchId ws)
      for_ [iid | (windowType -> Window.WouldFindEncounterCard iid _ _) <- ws] \iid ->
        spawnMyriadCopiesEngagedWith iid 1
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      -- "Reveal the Study."
      study <- selectJust $ locationIs Locations.study
      reveal study

      {- "Spawn the set-aside The Myriad Gentleman (Master of the House) and
      1[per_investigator] copies of The Myriad Gentleman (Thousandfold Man) at
      the Study." -}
      createSetAsideEnemy_ Enemies.theMyriadGentleman_043 study
      n <- perPlayer 1
      lead <- getLead
      spawnMyriadCopiesAt lead n study

      advanceActDeck attrs
      pure a
    _ -> JustOneMan <$> liftRunMessage msg attrs
