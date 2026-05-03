module Arkham.Treachery.Cards.ViolentOutburst (
  violentOutburst,
  ViolentOutburst (..),
)
where

import Arkham.Attack
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Trait (Trait (Humanoid))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ViolentOutburst = ViolentOutburst TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

violentOutburst :: TreacheryCard ViolentOutburst
violentOutburst = treachery ViolentOutburst Cards.violentOutburst

instance RunMessage ViolentOutburst where
  runMessage msg t@(ViolentOutburst attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      humanoids <- select $ NearestEnemyTo iid $ EnemyWithTrait Humanoid
      if null humanoids
        then findAndDrawEncounterCard iid $ #enemy <> CardWithTrait Humanoid
        else withLocationOf iid \lid ->
          chooseOneM iid do
            targets humanoids \humanoid -> do
              readyThis humanoid
              -- Force iid to be the prey for the duration of the move so any
              -- engagement check along the path picks iid (no choice prompt) when
              -- iid is at that location, and falls through to normal engagement
              -- otherwise.
              temporaryModifier humanoid attrs (ForcePrey $ Prey $ InvestigatorWithId iid) do
                moveUntil humanoid lid
              push
                $ IfEnemyExists
                  (enemyAtLocationWith iid <> EnemyWithId humanoid)
                  [ EnemyEngageInvestigator humanoid iid
                  , EnemyWillAttack $ enemyAttack humanoid attrs iid
                  ]
      pure t
    _ -> ViolentOutburst <$> liftRunMessage msg attrs
