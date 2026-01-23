module Arkham.Enemy.Cards.ThorneTheOneWithTheRedCravat (thorneTheOneWithTheRedCravat) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Campaigns.TheScarletKeys.Key.Cards qualified as Keys
import Arkham.Campaigns.TheScarletKeys.Key.Matcher
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyDefeated)
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Helpers.Query (getInvestigators, getLead)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Trait (Trait (Hazard))

newtype ThorneTheOneWithTheRedCravat = ThorneTheOneWithTheRedCravat EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thorneTheOneWithTheRedCravat :: EnemyCard ThorneTheOneWithTheRedCravat
thorneTheOneWithTheRedCravat = enemy ThorneTheOneWithTheRedCravat Cards.thorneTheOneWithTheRedCravat (4, Static 6, 4) (2, 1)

instance HasModifiersFor ThorneTheOneWithTheRedCravat where
  getModifiersFor (ThorneTheOneWithTheRedCravat a) = do
    n <- perPlayer 2
    modifySelf a [HealthModifier n]

instance HasAbilities ThorneTheOneWithTheRedCravat where
  getAbilities (ThorneTheOneWithTheRedCravat a) =
    extend
      a
      [ restricted a 1 (enemyWithScarletKey a)
          $ forced
          $ DrawCard #after Anyone (basic $ #treachery <> CardWithTrait Hazard) AnyDeck
      , restricted a 2 (thisExists a $ EnemyWithScarletKey $ scarletKeyIs Keys.theSableGlass)
          $ forced
          $ EnemyDefeated #when Anyone ByAny (be a)
      ]

instance RunMessage ThorneTheOneWithTheRedCravat where
  runMessage msg e@(ThorneTheOneWithTheRedCravat attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      lead <- getLead
      skeys <- select $ scarletKeyWithEnemy attrs.id
      chooseOneAtATimeM lead $ targets skeys shift
      pure e
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      lead <- getLead
      investigators <- getInvestigators
      theSableGlass <- selectJust $ scarletKeyIs Keys.theSableGlass
      chooseTargetM lead investigators $ push . PlaceScarletKey theSableGlass . AttachedToInvestigator
      pure e
    _ -> ThorneTheOneWithTheRedCravat <$> liftRunMessage msg attrs
