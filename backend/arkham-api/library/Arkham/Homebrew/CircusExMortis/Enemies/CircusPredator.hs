module Arkham.Homebrew.CircusExMortis.Enemies.CircusPredator (circusPredator) where

import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Location (getLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Trait (Trait (Woods))

newtype CircusPredator = CircusPredator EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

circusPredator :: EnemyCard CircusPredator
circusPredator =
  enemy CircusPredator Cards.circusPredator
    & setSpawnAt (LocationWithTrait Woods)

instance HasModifiersFor CircusPredator where
  getModifiersFor (CircusPredator a) = modifySelf a [AddKeyword Keyword.Hunter]

instance RunMessage CircusPredator where
  runMessage msg (CircusPredator attrs) = runQueueT $ case msg of
    -- Forced - After Circus Predator moves from a Woods location to another
    -- Woods location: it gets +1 horror until the end of the round. Capture
    -- the origin location before the move resolves (mirrors Innsmouth
    -- Shoggoth), since enemy-move windows do not carry the source location.
    EnemyMove eid lid | eid == attrs.id -> do
      mFrom <- getLocationOf eid
      attrs' <- liftRunMessage msg attrs
      woodsLocs <- select (LocationWithTrait Woods)
      for_ mFrom \from ->
        when (from `elem` woodsLocs && lid `elem` woodsLocs)
          $ roundModifier attrs attrs (HorrorDealt 1)
      pure (CircusPredator attrs')
    _ -> CircusPredator <$> liftRunMessage msg attrs
