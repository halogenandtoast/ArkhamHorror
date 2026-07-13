module Arkham.Enemy.Cards.NewMoonBeastTamerCircusExMortis (newMoonBeastTamerCircusExMortis) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.ForMovement
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelf)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Trait (Trait (Creature, Monster))

newtype NewMoonBeastTamerCircusExMortis = NewMoonBeastTamerCircusExMortis EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

newMoonBeastTamerCircusExMortis :: EnemyCard NewMoonBeastTamerCircusExMortis
newMoonBeastTamerCircusExMortis = enemy NewMoonBeastTamerCircusExMortis Cards.newMoonBeastTamerCircusExMortis

instance HasModifiersFor NewMoonBeastTamerCircusExMortis where
  getModifiersFor (NewMoonBeastTamerCircusExMortis a) = do
    modifySelf a [AddKeyword Keyword.Aloof, AddKeyword Keyword.Hunter]
    -- "if ready and unengaged" is a continuous condition; the +damage/+horror only
    -- manifests when an in-range enemy attacks (DamageDealt/HorrorDealt idiom, cf. Barn/UntouchedVault).
    whenM (a.id <=~> (ReadyEnemy <> UnengagedEnemy)) do
      let inRange = not_ (be a) <> at_ (orConnected NotForMovement $ locationWithEnemy a.id)
      modifySelect a inRange [DamageDealt 1]
      modifySelect a (inRange <> oneOf [EnemyWithTrait Creature, EnemyWithTrait Monster]) [HorrorDealt 1]

instance RunMessage NewMoonBeastTamerCircusExMortis where
  runMessage msg (NewMoonBeastTamerCircusExMortis attrs) =
    NewMoonBeastTamerCircusExMortis <$> runMessage msg attrs
