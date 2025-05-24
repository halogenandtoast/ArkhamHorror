module Arkham.Treachery.Cards.DismalCurse (dismalCurse) where

import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified_)
import Arkham.Helpers.SkillTest (getSkillTest, getSkillTestSource)
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection
import Arkham.Source
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype DismalCurse = DismalCurse TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dismalCurse :: TreacheryCard DismalCurse
dismalCurse = treachery DismalCurse Cards.dismalCurse

instance HasModifiersFor DismalCurse where
  getModifiersFor (DismalCurse a) = do
    getSkillTest >>= traverse_ \st -> do
      maybeModified_ a (SkillTestTarget st.id) do
        source <- MaybeT getSkillTestSource
        guard $ isSource a source
        pure [Difficulty 2]

instance RunMessage DismalCurse where
  runMessage msg t@(DismalCurse attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      horror <- field InvestigatorHorror iid
      sanity <- field InvestigatorSanity iid
      let damage = if horror > sanity * 2 then 4 else 2
      assignDamage iid attrs damage
      pure t
    _ -> DismalCurse <$> liftRunMessage msg attrs
