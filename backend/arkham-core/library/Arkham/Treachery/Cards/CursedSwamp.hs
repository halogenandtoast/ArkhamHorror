module Arkham.Treachery.Cards.CursedSwamp (CursedSwamp (..), cursedSwamp) where

import Arkham.Classes
import Arkham.Helpers.SkillTest
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Prelude
import Arkham.Source
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Helpers
import Arkham.Treachery.Runner

newtype CursedSwamp = CursedSwamp TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cursedSwamp :: TreacheryCard CursedSwamp
cursedSwamp = treachery CursedSwamp Cards.cursedSwamp

instance HasModifiersFor CursedSwamp where
  getModifiersFor (InvestigatorTarget iid) (CursedSwamp attrs) = do
    mSource <- getSkillTestSource
    case mSource of
      Just source | isSource attrs source -> do
        isBayou <- selectAny $ LocationWithTrait Bayou <> locationWithInvestigator iid
        pure $ toModifiers attrs [CannotCommitCards AnyCard | isBayou]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance RunMessage CursedSwamp where
  runMessage msg t@(CursedSwamp attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      push $ revelationSkillTest sid iid attrs #willpower (Fixed 3)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      push $ assignDamage iid attrs n
      pure t
    _ -> CursedSwamp <$> runMessage msg attrs
