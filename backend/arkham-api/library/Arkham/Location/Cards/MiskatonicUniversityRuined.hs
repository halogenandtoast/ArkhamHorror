module Arkham.Location.Cards.MiskatonicUniversityRuined (miskatonicUniversityRuined) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Ally))

newtype MiskatonicUniversityRuined = MiskatonicUniversityRuined LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

miskatonicUniversityRuined :: LocationCard MiskatonicUniversityRuined
miskatonicUniversityRuined =
  location MiskatonicUniversityRuined Cards.miskatonicUniversityRuined 5 (Static 1)

instance HasModifiersFor MiskatonicUniversityRuined where
  getModifiersFor (MiskatonicUniversityRuined a) = modifySelf a [CannotBeFullyFlooded]

instance HasAbilities MiskatonicUniversityRuined where
  getAbilities (MiskatonicUniversityRuined a) =
    extendRevealed1 a
      $ playerLimit PerGame
      $ restricted
        a
        1
        ( Here
            <> oneOf
              [ exists $ HealableInvestigator (a.ability 1) #horror (investigatorAt a)
              , exists $ HealableAsset (a.ability 1) #horror (assetAt a)
              ]
        )
      $ actionAbilityWithCost
      $ AtLeastOne (Fixed 3) (HandDiscardCost 1 $ basic $ mapOneOf CardWithSkillIcon [#intellect, #wild])

instance RunMessage MiskatonicUniversityRuined where
  runMessage msg l@(MiskatonicUniversityRuined attrs) = runQueueT $ case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 _ (totalDiscardCardPayments -> n) -> do
      doStep n msg
      pure l
    DoStep n msg'@(UseCardAbility iid (isSource attrs -> True) 1 _ _) | n > 0 -> do
      let source = attrs.ability 1
      investigators <- select $ HealableInvestigator source #horror (investigatorAt attrs)
      allies <- select $ HealableAsset source #horror (AssetWithTrait Ally <> assetAt attrs)
      unless (null investigators && null allies) do
        chooseOneM iid do
          targets investigators \i -> healHorror i source 1
          targets allies \asset -> healHorror asset source 1
        doStep (n - 1) msg'
      pure l
    _ -> MiskatonicUniversityRuined <$> liftRunMessage msg attrs
