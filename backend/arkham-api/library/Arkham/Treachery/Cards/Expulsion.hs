module Arkham.Treachery.Cards.Expulsion (expulsion) where

import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Projection
import Arkham.Trait (Trait (Cultist))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Expulsion = Expulsion TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

expulsion :: TreacheryCard Expulsion
expulsion = treachery Expulsion Cards.expulsion

instance RunMessage Expulsion where
  runMessage msg t@(Expulsion attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      nearestCultists <- select $ NearestEnemyTo iid $ EnemyWithTrait Cultist
      if null nearestCultists
        then gainSurge attrs
        else do
          mYourLocation <- selectOne $ locationWithInvestigator iid
          for_ mYourLocation \yourLocation -> do
            chooseOrRunOneM iid do
              targets nearestCultists \cultist -> do
                readyThis cultist
                moveUntil cultist yourLocation
                forTarget cultist msg
      pure t
    ForTarget (EnemyTarget cultist) (Revelation iid (isSource attrs -> True)) -> do
      whenM (matches cultist (enemyAtLocationWith iid)) do
        enemyEngageInvestigator cultist iid
        initiateEnemyAttack cultist (toSource attrs) iid
        ks <- fieldMap InvestigatorKeys toList iid
        for_ ks (placeKey cultist)
      pure t
    _ -> Expulsion <$> liftRunMessage msg attrs
