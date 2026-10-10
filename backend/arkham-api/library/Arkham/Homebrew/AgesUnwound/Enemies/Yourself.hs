module Arkham.Homebrew.AgesUnwound.Enemies.Yourself (yourself) where

import Arkham.ClassSymbol
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.AgesUnwound.Scenarios.TimeRunsOut.Helpers (yourselfOwner)
import Arkham.Investigator.Types (
  Field (InvestigatorBaseAgility, InvestigatorBaseCombat, InvestigatorClass),
 )
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Projection

newtype Yourself = Yourself EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | Act 2b: "Put the set-aside Yourself enemy next to the agenda deck. Each
investigator puts the top card of their deck facedown in their threat area, as a
copy of Yourself. 'You' on each copy of Yourself refers to the owner of the card."

Every clause on the card is read against that owner, which for a copy in a threat
area is simply the investigator whose threat area it sits in -- 'yourselfOwner'.
-}
yourself :: EnemyCard Yourself
yourself = enemy Yourself Cards.yourself

{- | "__Prey__ - You only. /
This enemy's fight value is equal to your base [combat]. /
This enemy's evade value is equal to your base [agility]. /
If you are a Guardian, this enemy gets +2 health. /
If you are a Seeker, this enemy gets +1 horror value. /
If you are a Rogue, this enemy gains alert. /
If you are a Mystic, place 1 doom on this enemy. /
If you are a Survivor, this enemy gets +1 damage value."

The printed fight and evade are @*@, which evaluates to @0@, so 'EnemyFight' and
'EnemyEvade' -- which the engine adds to the printed value -- land on exactly the
owner's base skill. @*@ stays on the card def so the browser prints it.

The Mystic clause is not a modifier: placing doom happens once, when the copy
enters play, and that is the @EnemySpawn@ handler below.
-}
instance HasModifiersFor Yourself where
  getModifiersFor (Yourself a) = do
    mowner <- yourselfOwner a.id
    for_ mowner \iid -> do
      combat <- field InvestigatorBaseCombat iid
      agility <- field InvestigatorBaseAgility iid
      klass <- field InvestigatorClass iid
      modifySelf a
        $ [EnemyFight combat, EnemyEvade agility, ForcePrey (OnlyPrey $ Prey $ InvestigatorWithId iid)]
        <> case klass of
          Guardian -> [HealthModifier 2]
          Seeker -> [HorrorDealt 1]
          Rogue -> [AddKeyword Keyword.Alert]
          Survivor -> [DamageDealt 1]
          _ -> []

instance RunMessage Yourself where
  runMessage msg (Yourself attrs) = runQueueT $ case msg of
    -- "If you are a Mystic, place 1 doom on this enemy."
    EnemySpawn details | details.enemy == attrs.id -> do
      mowner <- yourselfOwner attrs.id
      for_ mowner \iid -> do
        klass <- field InvestigatorClass iid
        when (klass == Mystic) $ placeDoom attrs attrs 1
      Yourself <$> liftRunMessage msg attrs
    _ -> Yourself <$> liftRunMessage msg attrs
