module Arkham.Treachery.Cards.TheSpinnerInDarkness (
  theSpinnerInDarkness,
  TheSpinnerInDarkness (..),
)
where

import Arkham.Ability
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest qualified as Msg
import Arkham.Matcher
import Arkham.SkillType (allSkills)
import Arkham.Trait (Trait (AncientOne))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Helpers qualified as Msg
import Arkham.Treachery.Import.Lifted

newtype TheSpinnerInDarkness = TheSpinnerInDarkness TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSpinnerInDarkness :: TreacheryCard TheSpinnerInDarkness
theSpinnerInDarkness = treachery TheSpinnerInDarkness Cards.theSpinnerInDarkness

instance HasModifiersFor TheSpinnerInDarkness where
  getModifiersFor (EnemyTarget eid) (TheSpinnerInDarkness attrs)
    | treacheryOnEnemy eid attrs =
        pure $ toModifiers attrs [DamageDealt 1, HorrorDealt 1]
  getModifiersFor _ _ = pure []

instance HasAbilities TheSpinnerInDarkness where
  getAbilities (TheSpinnerInDarkness attrs) = [restrictedAbility attrs 1 OnSameLocation actionAbility]

instance RunMessage TheSpinnerInDarkness where
  runMessage msg t@(TheSpinnerInDarkness attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      enemies <- select $ NearestEnemyTo iid $ EnemyWithTrait AncientOne
      when (notNull enemies) do
        chooseOrRunOne iid [targetLabel enemy [Msg.attachTreachery attrs enemy] | enemy <- enemies]
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseOne
        iid
        [SkillLabel s [Msg.beginSkillTest sid iid (attrs.ability 1) iid s (Fixed 5)] | s <- allSkills]

      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> TheSpinnerInDarkness <$> liftRunMessage msg attrs
