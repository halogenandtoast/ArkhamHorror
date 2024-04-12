module Arkham.Treachery.Cards.CaptiveMind (
  captiveMind,
  CaptiveMind (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Classes.HasGame
import Arkham.Helpers.SkillTest
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection
import Arkham.Source
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype CaptiveMind = CaptiveMind TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

captiveMind :: TreacheryCard CaptiveMind
captiveMind = treachery CaptiveMind Cards.captiveMind

doDiscard :: (HasGame m, HasQueue Message m) => InvestigatorId -> Source -> m ()
doDiscard iid source = do
  n <- getSkillTestModifiedSkillValue
  handCount <- fieldMap InvestigatorHand length iid
  let discardCount = max 0 (handCount - n)
  pushAll $ replicate discardCount $ toMessage $ chooseAndDiscardCard iid source

instance RunMessage CaptiveMind where
  runMessage msg t@(CaptiveMind attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ revelationSkillTest iid attrs #willpower (Fixed 0)
      pure t
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      doDiscard iid (toSource attrs)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      doDiscard iid (toSource attrs)
      pure t
    _ -> CaptiveMind <$> runMessage msg attrs
