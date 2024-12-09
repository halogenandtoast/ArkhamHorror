module Arkham.Treachery.Cards.Punishment (punishment, Punishment (..)) where

import Arkham.Ability
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest (getSkillTest, getSkillTestInvestigator, getSkillTestSource)
import Arkham.Matcher
import Arkham.Source
import Arkham.Trait (Trait (Witch))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted hiding (EnemyDefeated)

newtype Punishment = Punishment TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

punishment :: TreacheryCard Punishment
punishment = treachery Punishment Cards.punishment

instance HasModifiersFor Punishment where
  getModifiersFor (Punishment attrs) = do
    getSkillTest >>= \case
      Nothing -> pure mempty
      Just st -> maybeModified_ attrs (SkillTestTarget st.id) do
        source <- MaybeT getSkillTestSource
        investigator <- MaybeT getSkillTestInvestigator
        guard $ isSource attrs source && treacheryInThreatArea investigator attrs
        guardM
          . lift
          . selectAny
          $ ExhaustedEnemy
          <> EnemyWithTrait Witch
          <> enemyAtLocationWith investigator
        pure [SkillTestAutomaticallySucceeds]

instance HasAbilities Punishment where
  getAbilities (Punishment a) =
    [ skillTestAbility
        $ restrictedAbility a 1 (InThreatAreaOf You)
        $ forced
        $ EnemyDefeated #after Anyone ByAny AnyEnemy
    ]

instance RunMessage Punishment where
  runMessage msg t@(Punishment attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamage iid (attrs.ability 1) 1
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 2) attrs #willpower (Fixed 3)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> Punishment <$> liftRunMessage msg attrs
