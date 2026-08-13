module Arkham.Homebrew.DarkMatter.Enemies.Haita (haita) where

import Arkham.Enemy.Import.Lifted
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Cards

{- | The back of agenda 3, "Flight of the Byakhees" (@:dark-matter:156@); the
agenda's advance flips the card into her.

"Abomination. Humanoid. Elite.
Spawn - Entrance Tunnel.
Aloof. Retaliate. Hunter.
Forced - When you would evade a Byakhee enemy at Haïta's location: Deal 2 damage
to Haïta instead.
Victory 1."

Spawn, the keywords and Victory are all card data. The Forced clause is a
replacement effect on an evasion attempt — @AttemptToEvade \#when@ fires once the
evade's skill test is already being set up, and there is no primitive for
aborting it and substituting damage (the one core user of that window,
@Location/Cards/WesternRidge.hs@, only modifies the test). TODO(homebrew):
needs an evade-replacement seam.

This card previously carried agenda 3's "when a [[Brain]] story asset is
defeated" clause instead of its own text, which double-counted with the agenda
that actually prints it: both fired, removing the asset twice and adding 2
Impending Doom instead of 1.
-}
newtype Haita = Haita EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

haita :: EnemyCard Haita
haita = enemy Haita Cards.haita

instance RunMessage Haita where
  runMessage msg (Haita attrs) = Haita <$> runMessage msg attrs
