module Arkham.Homebrew.AgesUnwound.Acts.BigAndUgly_161 (bigAndUgly_161) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (CannotAttack), modifySelect)
import Arkham.Homebrew.AgesUnwound.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgesUnwound.Key
import Arkham.Matcher

newtype BigAndUgly_161 = BigAndUgly_161 ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | Act 3a on the front-hallway branch.

"Objective - Investigators at Cafeteria may spend the requisite number of clues
to advance" is the act's own cost, scoped by title: only investigators standing
in the Cafeteria may pay.
-}
bigAndUgly_161 :: ActCard BigAndUgly_161
bigAndUgly_161 =
  act (3, A) BigAndUgly_161 Cards.bigAndUgly_161
    $ Just
    $ GroupClueCost (PerPlayer 5) (LocationWithTitle "Cafeteria")

-- | "Whilst the Hound of Unmaking is undamaged, it cannot make attacks."
instance HasModifiersFor BigAndUgly_161 where
  getModifiersFor (BigAndUgly_161 a) =
    modifySelect
      a
      (enemyIs Enemies.houndOfUnmaking <> EnemyWithDamage (EqualTo $ Static 0))
      [CannotAttack]

-- | "Objective - If the Hound of Unmaking is defeated, advance."
instance HasAbilities BigAndUgly_161 where
  getAbilities (BigAndUgly_161 a) =
    [mkAbility a 1 $ Objective $ forced $ ifEnemyDefeated Enemies.houndOfUnmaking]

instance RunMessage BigAndUgly_161 where
  runMessage msg a@(BigAndUgly_161 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct aid _ advanceMode | aid == actId attrs && onSide B attrs -> do
      {- "If you spent clues to advance: Remove the Hound of Unmaking from the
      game."

      Nothing is recorded on either branch -- Scenario VI's campaign log entries
      are only the two resolutions. If you defeated it instead it is already in
      the victory display. -}
      when (advanceMode == AdvancedWithClues) do
        selectEach (enemyIs Enemies.houndOfUnmaking) removeFromGame

      {- "However you advanced: Put each set-aside Ritual Circle location into
      play. Check your Campaign Log. If /the investigators eliminated the high
      priest/ is *not* recorded, spawn the set-aside The Myriad Gentleman (The
      High Priest) at any Ritual Circle location (/A Harnessed Future/, if
      possible)."

      "Each set-aside Ritual Circle" is this scenario's own /The Present,
      Fractured/ plus whichever of the @night_of_the_ritual@ pair setup left in the
      pool -- setup removes the one the investigators stepped into. -}
      placeSetAsideLocations_ [Locations.ritualCircle_173]
      for_ [Locations.ritualCircle_224, Locations.ritualCircle_225] \def ->
        whenM (selectAny $ SetAsideCardMatch $ cardIs def) $ placeSetAsideLocation_ def

      unlessM (getHasRecord TheInvestigatorsEliminatedTheHighPriest) do
        {- His movement ban means a spawn anywhere non-[[Ritual]] strands him, so
        the fallback stays inside the Ritual circles. -}
        harnessedFuture <- selectOne $ locationIs Locations.ritualCircle_224
        circles <- select $ LocationWithTitle "Ritual Circle"
        for_ (harnessedFuture <|> listToMaybe circles)
          $ createSetAsideEnemy_ Enemies.theMyriadGentleman_233

      -- "Advance to Act 4a - Breaking the Circles."
      advanceToAct attrs Cards.breakingTheCircles A
      pure a
    _ -> BigAndUgly_161 <$> liftRunMessage msg attrs
