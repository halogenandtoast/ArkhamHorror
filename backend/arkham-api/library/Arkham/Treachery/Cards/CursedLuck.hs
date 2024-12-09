module Arkham.Treachery.Cards.CursedLuck (CursedLuck (..), cursedLuck) where

import Arkham.Ability
import Arkham.Classes
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Helpers
import Arkham.Treachery.Runner

newtype CursedLuck = CursedLuck TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cursedLuck :: TreacheryCard CursedLuck
cursedLuck = treachery CursedLuck Cards.cursedLuck

instance HasModifiersFor CursedLuck where
  getModifiersFor (CursedLuck attrs) = do
    getSkillTest >>= \case
      Nothing -> pure mempty
      Just _ -> inThreatAreaGets attrs [AnySkillValue (-1)]

instance HasAbilities CursedLuck where
  getAbilities (CursedLuck x) =
    [ restrictedAbility x 1 (InThreatAreaOf You)
        $ forced
        $ SkillTestResult #after You AnySkillTest (SuccessResult $ atLeast 1)
    ]

instance RunMessage CursedLuck where
  runMessage msg t@(CursedLuck attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> CursedLuck <$> runMessage msg attrs
