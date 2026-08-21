module Arkham.Treachery.Cards.ChildrenOfBlood.BloodBlight.GrislyCompulsion (grislyCompulsion) where

import Arkham.Calculation
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Trait (Trait (Ally))
import Arkham.Treachery.CardDefs.ChildrenOfBlood.BloodBlight qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype GrislyCompulsion = GrislyCompulsion TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grislyCompulsion :: TreacheryCard GrislyCompulsion
grislyCompulsion = treachery GrislyCompulsion Cards.grislyCompulsion

instance RunMessage GrislyCompulsion where
  runMessage msg t@(GrislyCompulsion attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      skillTestModifier sid attrs (SkillTestTarget sid)
        $ CalculatedDifficulty
        $ CountChaosTokens
        $ SealedOnInvestigator (InvestigatorWithId iid) #blood
      revelationSkillTest sid iid attrs #willpower (Fixed 2)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      investigators <- select $ colocatedWith iid
      allies <- select $ AssetWithTrait Ally <> assetAtLocationWith iid
      chooseOneM iid do
        targets investigators \iid' -> assignDamage iid' attrs 2
        targets allies \aid -> dealAssetDamage aid attrs 2
      pure t
    _ -> GrislyCompulsion <$> liftRunMessage msg attrs
