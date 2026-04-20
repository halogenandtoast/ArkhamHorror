module Arkham.Treachery.Cards.RedHerring (redHerring) where

import Arkham.Helpers.SkillTest.Lifted (beginSkillTest)
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection
import Arkham.Token qualified as Token
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted hiding (beginSkillTest)

newtype RedHerring = RedHerring TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

redHerring :: TreacheryCard RedHerring
redHerring = treachery RedHerring Cards.redHerring

instance RunMessage RedHerring where
  runMessage msg t@(RedHerring attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      clueCount <- field InvestigatorClues iid
      let difficulty = if clueCount >= 2 then Fixed 4 else Fixed 2
      sid <- getRandom
      beginSkillTest sid iid (toSource attrs) iid #intellect difficulty
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignHorror iid attrs 1
      mLoc <- field InvestigatorLocation iid
      case mLoc of
        Nothing -> pure ()
        Just loc -> moveTokens (toSource attrs) (toSource iid) (toTarget loc) Token.Clue 1
      pure t
    _ -> RedHerring <$> liftRunMessage msg attrs
