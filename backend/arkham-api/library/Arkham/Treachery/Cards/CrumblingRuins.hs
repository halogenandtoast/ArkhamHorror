module Arkham.Treachery.Cards.CrumblingRuins (crumblingRuins) where

import Arkham.Investigator.Types (Field (..))
import Arkham.Key
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype CrumblingRuins = CrumblingRuins TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crumblingRuins :: TreacheryCard CrumblingRuins
crumblingRuins = treachery CrumblingRuins Cards.crumblingRuins

instance RunMessage CrumblingRuins where
  runMessage msg t@(CrumblingRuins attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      ks <-
        concatMap (mapMaybe (fmap (.face) . preview _TokenKey) . toList)
          <$> selectField LocationKeys (locationWithInvestigator iid)
      iks <-
        concatMap (mapMaybe (fmap (.face) . preview _TokenKey) . toList)
          <$> selectField InvestigatorKeys (colocatedWith iid)
      onRevealChaosTokenEffect sid (mapOneOf ChaosTokenFaceIs (nub $ ks <> iks)) attrs attrs failSkillTest
      revelationSkillTest sid iid attrs #agility (Fixed 3)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n | n > 0 -> do
      assignDamage iid attrs n
      pure t
    _ -> CrumblingRuins <$> liftRunMessage msg attrs
