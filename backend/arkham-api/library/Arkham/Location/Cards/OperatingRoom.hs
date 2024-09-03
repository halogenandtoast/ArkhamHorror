module Arkham.Location.Cards.OperatingRoom (operatingRoom, OperatingRoom (..)) where

import Arkham.GameValue
import Arkham.Helpers.Investigator
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype OperatingRoom = OperatingRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

operatingRoom :: LocationCard OperatingRoom
operatingRoom = location OperatingRoom Cards.operatingRoom 2 (PerPlayer 1)

instance HasAbilities OperatingRoom where
  getAbilities (OperatingRoom attrs) =
    withRevealedAbilities
      attrs
      [skillTestAbility $ restrictedAbility attrs 1 Here $ ActionAbility [] $ ActionCost 2]

instance RunMessage OperatingRoom where
  runMessage msg l@(OperatingRoom attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigators <- select $ affectsOthers $ investigatorAt attrs.id
      player <- getPlayer iid
      sid <- getRandom
      push
        $ chooseOrRunOne
          player
          [ targetLabel target [beginSkillTest sid iid (attrs.ability 1) target #intellect (Fixed 4)]
          | target <- investigators
          ]
      pure l
    PassedThisSkillTest _iid (isAbilitySource attrs 1 -> True) -> do
      mTarget <- getSkillTestTarget
      case mTarget of
        Just (InvestigatorTarget target) ->
          whenM (canHaveDamageHealed (attrs.ability 1) target)
            $ push
            $ HealDamage (toTarget target) (attrs.ability 1) 3
        _ -> error "invalid target"
      pure l
    FailedThisSkillTest _iid (isAbilitySource attrs 1 -> True) -> do
      mTarget <- getSkillTestTarget
      case mTarget of
        Just (InvestigatorTarget target) -> push $ assignDamage target (attrs.ability 1) 2
        _ -> error "invalid target"
      pure l
    _ -> OperatingRoom <$> runMessage msg attrs
