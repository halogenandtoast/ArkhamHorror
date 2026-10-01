{- | The Prophecy Fulfilled (:193) and The Prophecy Unfulfilled (:194) are the same
card apart from the damage threshold on their first Forced, so everything but that
number lives here. Both are implemented as agendas only; the act deck is empty once
act 1 is removed from the game.
-}
module Arkham.Homebrew.CircusExMortis.Agendas.TheProphecy (
  prophecyAbilities,
  removeShubNiggurathsDamage,
  removeDoomFromEveryOtherCard,
  shubNiggurathAttacks,
) where

import Arkham.Ability
import Arkham.Agenda.Import.Lifted
import Arkham.Enemy.Types (Field (EnemyDamage))
import Arkham.Helpers.Doom (getDoomOnTarget, targetsWithDoom)
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.CircusExMortis.Helpers (scenarioI18n)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection

shubNiggurath :: EnemyMatcher
shubNiggurath = enemyIs Enemies.shubNiggurath

prophecyAbilities :: AgendaAttrs -> [Ability]
prophecyAbilities a =
  [ -- "Forced - When the investigation phase ends: Remove all damage from
    -- Shub-Niggurath. If fewer than X[per_investigator] damage was removed, ready
    -- Shub-Niggurath." With no damage to remove and Shub-Niggurath already ready
    -- there is nothing for the Forced to do, so it is not triggered at all.
    restricted a 1 (exists $ shubNiggurath <> oneOf [EnemyWithDamage (atLeast 1), ExhaustedEnemy])
      $ forced
      $ PhaseEnds #when #investigation
  , -- "Forced - At the end of the round: Remove all doom from each other card in
    -- play. ..." This agenda is not an "other card", so its own doom is what
    -- accumulates toward the threshold; with no doom anywhere else the Forced is
    -- not triggered.
    restricted a 2 (exists $ TargetWithDoom <> not_ (targetIs a))
      $ forced
      $ RoundEnds #when
  , -- "Forced - At the end of the enemy phase, if there are no investigators at
    -- Silent Clearing, advance." Advancing is a loss (the back is R1).
    restricted a 3 (notExists $ InvestigatorAt (locationIs Locations.silentClearing))
      $ forced
      $ PhaseEnds #when #enemy
  , -- "Objective - When this agenda would advance by reaching its doom threshold:
    -- Instead, resolve Resolution 2."
    mkAbility a 4 $ Objective $ forced $ AgendaWouldAdvance #when #doom (be a)
  ]

{- | "Remove all damage from Shub-Niggurath. If fewer than @n@[per_investigator] damage
was removed, ready Shub-Niggurath." The amount removed is whatever was on it, read
before the removal.
-}
removeShubNiggurathsDamage :: (ReverseQueue m, Sourceable source) => Int -> source -> m ()
removeShubNiggurathsDamage n source = selectForMaybeM shubNiggurath \shub -> do
  threshold <- perPlayer n
  damage <- field EnemyDamage shub
  healAllDamage source shub
  when (damage < threshold) $ readyThis shub

{- | "Remove all doom from each other card in play. For each doom removed,
Shub-Niggurath makes an immediate attack against an investigator, regardless of
location." Each attack is deferred so the choice of target is recomputed against the
state the previous attack left behind.

'targetsWithDoom' rather than @select TargetWithDoom@: the latter also hands back the
enemy-location proxy of a location, which would count that location's doom twice and
owe an extra attack.
-}
removeDoomFromEveryOtherCard :: ReverseQueue m => AgendaAttrs -> Message -> m ()
removeDoomFromEveryOtherCard attrs msg = do
  elsewhere <- filter (/= toTarget attrs) <$> targetsWithDoom
  removed <- sum <$> traverse getDoomOnTarget elsewhere
  for_ elsewhere $ removeAllDoom (attrs.ability 2)
  replicateM_ removed $ doStep 1 msg

{- | One of the attacks owed by 'removeDoomFromEveryOtherCard'. "Regardless of
location" is the default for an attack initiated this way: nothing about engagement
or colocation is checked, and 'Massive' does not widen a single-target attack.
-}
shubNiggurathAttacks :: ReverseQueue m => AgendaAttrs -> m ()
shubNiggurathAttacks attrs = selectForMaybeM shubNiggurath \shub -> do
  investigators <- select Anyone
  unless (null investigators) do
    leadChooseOneM $ scenarioI18n "thousandToOne" $ scope "theProphecy" do
      questionLabeled "shubNiggurathAttacks"
      targets investigators $ initiateEnemyAttack shub (attrs.ability 2)
