module Arkham.Treachery.Cards.ViolentOutburst (
  violentOutburst,
  ViolentOutburst (..),
)
where

import Arkham.Prelude

import Arkham.Attack
import Arkham.Classes
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.Trait (Trait (Humanoid))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype ViolentOutburst = ViolentOutburst TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

violentOutburst :: TreacheryCard ViolentOutburst
violentOutburst = treachery ViolentOutburst Cards.violentOutburst

instance RunMessage ViolentOutburst where
  runMessage msg t@(ViolentOutburst attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      humanoids <- selectList $ NearestEnemy $ EnemyWithTrait Humanoid
      if null humanoids
        then push $ findAndDrawEncounterCard iid $ #enemy <> CardWithTrait Humanoid
        else do
          player <- getPlayer iid
          mlid <- field InvestigatorLocation iid
          for_ mlid $ \lid -> do
            pushAll
              [ chooseOne player
                  $ [ targetLabel
                      humanoid
                      [ Ready (toTarget humanoid)
                      , MoveUntil lid (toTarget humanoid)
                      , IfEnemyExists
                          (enemyAtLocationWith iid <> EnemyWithId humanoid)
                          [ EnemyEngageInvestigator humanoid iid
                          , EnemyWillAttack $ enemyAttack humanoid attrs iid
                          ]
                      ]
                    | humanoid <- humanoids
                    ]
              ]
      pure t
    _ -> ViolentOutburst <$> runMessage msg attrs
