module Arkham.Homebrew.DarkMatter.Enemies.Haita (haita) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Locations
import Arkham.Matcher
import Arkham.Trait (Trait (Byakhee))

{- | Brought into play by agenda 3, "Flight of the Byakhees" (@:dark-matter:156@),
whose advance creates her at the Entrance Tunnel.

"Abomination. Humanoid. Elite.
Spawn - Entrance Tunnel.
Aloof. Retaliate. Hunter.
Forced - When you would evade a Byakhee enemy at Haïta's location: Deal 2 damage
to Haïta instead.
Victory 1."

The keywords, traits and Victory are card data.

This card previously carried agenda 3's "when a [[Brain]] story asset is
defeated" clause instead of its own text, which double-counted with the agenda
that actually prints it: both fired, removing the asset twice and adding 2
Impending Doom instead of 1.
-}
newtype Haita = Haita EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

haita :: EnemyCard Haita
haita = enemy Haita Cards.haita & setSpawnAt (locationIs Locations.entranceTunnel)

instance HasAbilities Haita where
  getAbilities (Haita a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ EnemyWouldBeEvaded #when You (EnemyWithTrait Byakhee <> EnemyAt (locationWithEnemy a))

instance RunMessage Haita where
  runMessage msg e@(Haita attrs) = runQueueT $ case msg of
    -- "instead": cancelling the would-batch leaves the Byakhee ready and engaged.
    UseCardAbility iid (isSource attrs -> True) 1 ws _ -> do
      cancelWindowBatch ws
      nonAttackEnemyDamage (Just iid) (attrs.ability 1) 2 attrs
      pure e
    _ -> Haita <$> liftRunMessage msg attrs
