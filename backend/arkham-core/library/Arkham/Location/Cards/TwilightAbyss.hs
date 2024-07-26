module Arkham.Location.Cards.TwilightAbyss (
  twilightAbyss,
  TwilightAbyss (..),
) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Timing qualified as Timing

newtype TwilightAbyss = TwilightAbyss LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

twilightAbyss :: LocationCard TwilightAbyss
twilightAbyss = location TwilightAbyss Cards.twilightAbyss 2 (PerPlayer 2)

instance HasAbilities TwilightAbyss where
  getAbilities (TwilightAbyss a) =
    withRevealedAbilities
      a
      [ skillTestAbility $ mkAbility a 1 $ ForcedAbility $ Enters Timing.After You $ LocationWithId $ toId a
      ]

instance RunMessage TwilightAbyss where
  runMessage msg l@(TwilightAbyss attrs) = case msg of
    Msg.RevealLocation _ lid | lid == toId attrs -> do
      TwilightAbyss <$> runMessage msg (attrs & labelL .~ "twilightAbyss")
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      sid <- getRandom
      let skillTest sType = beginSkillTest sid iid (attrs.ability 1) iid sType (Fixed 3)
      player <- getPlayer iid
      push $ chooseOne player [SkillLabel sType [skillTest sType] | sType <- [#combat, #agility]]
      pure l
    FailedSkillTest iid _ (isAbilitySource attrs 1 -> True) SkillTestInitiatorTarget {} _ n -> do
      push $ InvestigatorAssignDamage iid (toAbilitySource attrs 1) DamageAny n 0
      pure l
    _ -> TwilightAbyss <$> runMessage msg attrs
