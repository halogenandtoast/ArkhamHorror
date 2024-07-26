module Arkham.Treachery.Cards.Overgrowth (overgrowth, Overgrowth (..)) where

import Arkham.Ability
import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.Helpers.Investigator
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype Overgrowth = Overgrowth TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

overgrowth :: TreacheryCard Overgrowth
overgrowth = treachery Overgrowth Cards.overgrowth

instance HasModifiersFor Overgrowth where
  getModifiersFor (InvestigatorTarget iid) (Overgrowth attrs) = do
    mlid <- getMaybeLocation iid
    pure $ toModifiers attrs [CannotExplore | lid <- toList mlid, treacheryOnLocation lid attrs]
  getModifiersFor _ _ = pure []

instance HasAbilities Overgrowth where
  getAbilities (Overgrowth a) = [skillTestAbility $ restrictedAbility a 1 OnSameLocation actionAbility]

instance RunMessage Overgrowth where
  runMessage msg t@(Overgrowth attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      mlid <- getMaybeLocation iid
      for_ mlid \lid -> do
        withoutOvergrowth <- lid <=~> LocationWithoutTreachery (treacheryIs Cards.overgrowth)
        pushWhen withoutOvergrowth $ attachTreachery attrs lid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      let chooseSkillTest sType = SkillLabel sType [beginSkillTest sid iid (attrs.ability 1) attrs sType (Fixed 4)]
      player <- getPlayer iid
      push $ chooseOne player $ map chooseSkillTest [#combat, #intellect]
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      push $ toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> Overgrowth <$> runMessage msg attrs
