module Arkham.Location.Cards.CoterieLibraryLair (coterieLibraryLair) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Campaigns.TheScarletKeys.Key.Matcher
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Trait (Trait (Coterie))

newtype CoterieLibraryLair = CoterieLibraryLair LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

coterieLibraryLair :: LocationCard CoterieLibraryLair
coterieLibraryLair = location CoterieLibraryLair Cards.coterieLibraryLair 4 (PerPlayer 1)

instance HasAbilities CoterieLibraryLair where
  getAbilities (CoterieLibraryLair a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> exists (EnemyWithTrait Coterie <> EnemyWithAnyScarletKey))
      $ forced
      $ SkillTestResult #after You (WhileInvestigating $ be a) #failure

instance RunMessage CoterieLibraryLair where
  runMessage msg l@(CoterieLibraryLair attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <- select $ NearestEnemyTo iid $ EnemyWithTrait Coterie <> EnemyWithAnyScarletKey
      chooseTargetM iid enemies $ \eid -> do
        skeys <- select $ ScarletKeyWithPlacement (AttachedToEnemy eid)
        chooseOneAtATimeM iid $ targets skeys shift
      pure l
    _ -> CoterieLibraryLair <$> liftRunMessage msg attrs
