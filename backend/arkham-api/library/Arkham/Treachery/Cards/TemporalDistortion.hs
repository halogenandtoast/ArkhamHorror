module Arkham.Treachery.Cards.TemporalDistortion (temporalDistortion) where

import Arkham.Ability
import Arkham.Helpers.Location (getLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.I18n
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Trait (Trait (Future, Past, Present))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype TemporalDistortion = TemporalDistortion TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

temporalDistortion :: TreacheryCard TemporalDistortion
temporalDistortion = treachery TemporalDistortion Cards.temporalDistortion

instance HasModifiersFor TemporalDistortion where
  getModifiersFor (TemporalDistortion a) =
    modifySelect a Anyone [CannotTriggerAbilityMatching (AbilityOnStory (StoryMatchAll []))]

instance HasAbilities TemporalDistortion where
  getAbilities (TemporalDistortion a) = [mkAbility a 1 actionAbility]

instance RunMessage TemporalDistortion where
  runMessage msg t@(TemporalDistortion attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      agenda <- selectJust AnyAgenda
      attachTreachery attrs agenda
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      getLocationOf iid >>= traverse_ \lid -> do
        traits <- field LocationTraits lid
        sid <- getRandom
        chooseOneM iid $ withI18n do
          when (Past `member` traits)
            $ chooseTest #willpower 2
            $ beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 2)
          when (Present `member` traits)
            $ chooseTest #agility 3
            $ beginSkillTest sid iid (attrs.ability 1) iid #agility (Fixed 3)
          when (Future `member` traits)
            $ chooseTest #combat 4
            $ beginSkillTest sid iid (attrs.ability 1) iid #combat (Fixed 4)
      pure t
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      toDiscard (attrs.ability 1) attrs
      pure t
    _ -> TemporalDistortion <$> liftRunMessage msg attrs
