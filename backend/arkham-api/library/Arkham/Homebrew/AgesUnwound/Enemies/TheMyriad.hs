module Arkham.Homebrew.AgesUnwound.Enemies.TheMyriad (theMyriad) where

import Arkham.Enemy.Import.Lifted
import Arkham.Enemy.Types (Field (EnemyLocation))
import Arkham.Helpers.Modifiers (ModifierType (EnemyAttacksOverride), modifySelf)
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Locations
import Arkham.Matcher
import Arkham.Projection

newtype TheMyriad = TheMyriad EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

{- | /The Myriad (Weapon Without Form)/, the reverse of the @:ages-unwound:190@
printing of /Fulcrum of Possibility/. __Spawn__ - The Present.

__Massive__ and __Swarming 2[per_investigator]__ are printed keywords and come
off the card def.
-}
theMyriad :: EnemyCard TheMyriad
theMyriad = enemyWith TheMyriad Cards.theMyriad (spawnAtL ?~ SpawnAt (locationIs Locations.thePresent))

{- | "__Forced__ - When Fulcrum of Possibility attacks during the enemy phase:
Resolve its attack against one investigator at its location /(instead of against
against each investigator)./ Attacks from copies of Fulcrum of Possibility must be
split as evenly as possible between each investigator at its location."

(The card names itself by the location it is printed on the back of.)

A __Massive__ enemy's enemy-phase attack is built against everyone at its
location, and 'EnemyAttacksOverride' is the engine's seam for narrowing that set
-- so this is a modifier rather than a __Forced__ ability, which is also the only
way the override can be in place before @Do EnemiesAttack@ constructs the attack.

"Split as evenly as possible" is a round robin: the copies at a location are
numbered in the order the engine lists them and each takes the investigator that
many steps along the turn order, so with two copies and two investigators each
investigator is attacked once, and with three copies and two investigators one is
attacked twice. Nothing is left to a prompt, because an even split is not a
choice.
-}
instance HasModifiersFor TheMyriad where
  getModifiersFor (TheMyriad a) = do
    mlid <- field EnemyLocation a.id
    for_ mlid \lid -> do
      iids <- select $ investigatorAt lid
      copies <- select $ enemyIs Cards.theMyriad <> EnemyAt (LocationWithId lid)
      let idx = length $ takeWhile (/= a.id) copies
      case drop (idx `mod` max 1 (length iids)) iids of
        target : _ -> modifySelf a [EnemyAttacksOverride (InvestigatorWithId target)]
        [] -> pure ()

instance RunMessage TheMyriad where
  runMessage msg (TheMyriad attrs) = TheMyriad <$> runQueueT (liftRunMessage msg attrs)
