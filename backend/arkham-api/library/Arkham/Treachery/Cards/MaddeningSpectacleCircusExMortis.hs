module Arkham.Treachery.Cards.MaddeningSpectacleCircusExMortis (
  maddeningSpectacleCircusExMortis,
) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.SkillType
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Metadata = Metadata {chosenSkill :: Maybe SkillType}
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON)

newtype MaddeningSpectacleCircusExMortis = MaddeningSpectacleCircusExMortis (TreacheryAttrs `With` Metadata)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

maddeningSpectacleCircusExMortis :: TreacheryCard MaddeningSpectacleCircusExMortis
maddeningSpectacleCircusExMortis =
  treachery
    (MaddeningSpectacleCircusExMortis . (`with` Metadata Nothing))
    Cards.maddeningSpectacleCircusExMortis

instance HasModifiersFor MaddeningSpectacleCircusExMortis where
  getModifiersFor (MaddeningSpectacleCircusExMortis (attrs `With` meta)) =
    for_ ((,) <$> attrs.inThreatAreaOf <*> chosenSkill meta) \(iid, sk) ->
      modified_ attrs iid [SkillModifier sk (-1)]

instance HasAbilities MaddeningSpectacleCircusExMortis where
  getAbilities (MaddeningSpectacleCircusExMortis (x `With` _)) =
    [restricted x 1 (InThreatAreaOf You) actionAbility]

instance RunMessage MaddeningSpectacleCircusExMortis where
  runMessage msg t@(MaddeningSpectacleCircusExMortis (attrs `With` meta)) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      willpower <- field InvestigatorWillpower iid
      intellect <- field InvestigatorIntellect iid
      combat <- field InvestigatorCombat iid
      agility <- field InvestigatorAgility iid
      let skills = [(#willpower, willpower), (#intellect, intellect), (#combat, combat), (#agility, agility)]
          highest = maximum (impureNonNull (map snd skills))
          tied = [sk | (sk, v) <- skills, v == highest]
      chooseOrRunOneM iid do
        for_ tied \sk ->
          skillLabeled sk
            $ push
            $ UseCardAbilityChoice iid (toSource attrs) 1 (SkillChoiceMetadata sk) [] NoPayment
      pure t
    UseCardAbilityChoice _ (isSource attrs -> True) 1 (SkillChoiceMetadata sk) _ _ -> do
      pure $ MaddeningSpectacleCircusExMortis $ attrs `with` Metadata (Just sk)
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      let others = maybe allSkills (\sk -> filter (/= sk) allSkills) (chosenSkill meta)
      chooseOneM iid do
        for_ others \sk ->
          skillLabeled sk $ beginSkillTest sid iid (attrs.ability 1) iid sk (Fixed 3)
      pure t
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> MaddeningSpectacleCircusExMortis . (`with` meta) <$> liftRunMessage msg attrs
