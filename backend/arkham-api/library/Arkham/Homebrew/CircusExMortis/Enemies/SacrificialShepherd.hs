module Arkham.Homebrew.CircusExMortis.Enemies.SacrificialShepherd (sacrificialShepherd) where

import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectWhen, modifySelf)
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Cards
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Trait (Trait (Bystander))

newtype SacrificialShepherd = SacrificialShepherd EnemyAttrs
  deriving anyclass (IsEnemy, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

sacrificialShepherd :: EnemyCard SacrificialShepherd
sacrificialShepherd =
  enemy SacrificialShepherd Cards.sacrificialShepherd
    & setSpawnAtFirst [LocationWithAsset (withTrait Bystander), Anywhere]

instance HasModifiersFor SacrificialShepherd where
  getModifiersFor (SacrificialShepherd a) = do
    let loc = locationWithEnemy a
    bystanderPresent <- selectAny $ AssetAt loc <> AssetWithTrait Bystander
    modifySelectWhen
      a
      a.ready
      (InvestigatorAt loc)
      [CannotTriggerAbilityMatching $ AbilityOnCard (CardWithTrait Bystander)]
    unless bystanderPresent
      $ modifySelf a [AddKeyword Keyword.Hunter, EnemyEvade 1, DamageDealt 1]
