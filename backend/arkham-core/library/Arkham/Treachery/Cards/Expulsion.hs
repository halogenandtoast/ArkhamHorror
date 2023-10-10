module Arkham.Treachery.Cards.Expulsion (
  expulsion,
  Expulsion (..),
)
where

import Arkham.Prelude

import Arkham.Attack
import Arkham.Classes
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Trait (Trait (Cultist))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype Expulsion = Expulsion TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

expulsion :: TreacheryCard Expulsion
expulsion = treachery Expulsion Cards.expulsion

instance RunMessage Expulsion where
  runMessage msg t@(Expulsion attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      nearestCultists <- selectList $ NearestEnemy $ EnemyWithTrait Cultist
      if null nearestCultists
        then push $ gainSurge attrs
        else do
          mYourLocation <- selectOne $ locationWithInvestigator iid
          ks <- fieldMap InvestigatorKeys toList iid
          for_ mYourLocation $ \yourLocation -> do
            player <- getPlayer iid
            push
              $ chooseOrRunOne
                player
                [ targetLabel
                  cultist
                  $ [ MoveUntil yourLocation (toTarget cultist)
                    , EnemyEngageInvestigator cultist iid
                    , InitiateEnemyAttack $ enemyAttack cultist (toSource attrs) iid
                    ]
                  <> [PlaceKey (toTarget cultist) k | k <- ks]
                | cultist <- nearestCultists
                ]
      pure t
    _ -> Expulsion <$> runMessage msg attrs
