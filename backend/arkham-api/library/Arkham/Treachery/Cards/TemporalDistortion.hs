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

testedSkills :: TreacheryAttrs -> [Text]
testedSkills attrs = toResultDefault [] attrs.meta

chooseTemporalTest :: ReverseQueue m => InvestigatorId -> TreacheryAttrs -> m ()
chooseTemporalTest iid attrs = do
  getLocationOf iid >>= traverse_ \lid -> do
    traits <- field LocationTraits lid
    let available =
          [("willpower", #willpower, 2) | Past `member` traits, "willpower" `notElem` testedSkills attrs]
            <> [("agility", #agility, 3) | Present `member` traits, "agility" `notElem` testedSkills attrs]
            <> [("combat", #combat, 4) | Future `member` traits, "combat" `notElem` testedSkills attrs]
    chooseOneM iid $ withI18n do
      labeled' "doNotTest" nothing
      for_ available \(label, skill, difficulty) ->
        chooseTest skill difficulty
          $ handleTarget iid attrs
          $ LabeledTarget label (toTarget attrs)

instance RunMessage TemporalDistortion where
  runMessage msg t@(TemporalDistortion attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      agenda <- selectJust AnyAgenda
      attachTreachery attrs agenda
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseTemporalTest iid attrs
      pure t
    HandleTargetChoice iid (isSource attrs -> True) (LabeledTarget label _) -> do
      let (skill, difficulty) = case label of
            "willpower" -> (#willpower, 2)
            "agility" -> (#agility, 3)
            "combat" -> (#combat, 4)
            _ -> error "invalid temporal distortion skill"
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid skill (Fixed difficulty)
      pure $ TemporalDistortion $ attrs & setMeta (label : testedSkills attrs)
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      toDiscard (attrs.ability 1) attrs
      pure t
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      chooseTemporalTest iid attrs
      pure t
    _ -> TemporalDistortion <$> liftRunMessage msg attrs
