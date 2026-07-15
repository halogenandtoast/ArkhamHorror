module Arkham.Homebrew.CircusExMortis.Treacheries.MaddeningSpectacle (
  maddeningSpectacle,
) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.SkillType
import Arkham.Homebrew.CircusExMortis.CardDefs.Treacheries qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Metadata = Metadata {chosenSkill :: Maybe SkillType}
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON)

newtype MaddeningSpectacle = MaddeningSpectacle (TreacheryAttrs `With` Metadata)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

maddeningSpectacle :: TreacheryCard MaddeningSpectacle
maddeningSpectacle =
  treachery
    (MaddeningSpectacle . (`with` Metadata Nothing))
    Cards.maddeningSpectacle

instance HasModifiersFor MaddeningSpectacle where
  getModifiersFor (MaddeningSpectacle (attrs `With` meta)) =
    for_ ((,) <$> attrs.inThreatAreaOf <*> chosenSkill meta) \(iid, sk) ->
      modified_ attrs iid [SkillModifier sk (-1)]

instance HasAbilities MaddeningSpectacle where
  getAbilities (MaddeningSpectacle (x `With` _)) =
    [restricted x 1 (InThreatAreaOf You) actionAbility]

instance RunMessage MaddeningSpectacle where
  runMessage msg t@(MaddeningSpectacle (attrs `With` meta)) = runQueueT $ case msg of
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
      pure $ MaddeningSpectacle $ attrs `with` Metadata (Just sk)
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
    _ -> MaddeningSpectacle . (`with` meta) <$> liftRunMessage msg attrs
