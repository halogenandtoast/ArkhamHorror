module Arkham.Location.Cards.WitchHouseRuins (witchHouseRuins) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.GameValue
import Arkham.Helpers.SkillTest.Lifted (investigateLocation_)
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Message qualified as Msg

newtype WitchHouseRuins = WitchHouseRuins LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

witchHouseRuins :: LocationCard WitchHouseRuins
witchHouseRuins = location WitchHouseRuins Cards.witchHouseRuins 2 (Static 0)

instance HasAbilities WitchHouseRuins where
  getAbilities (WitchHouseRuins a) =
    extendRevealed
      a
      [ playerLimit PerGame $ investigateAbility a 1 mempty Here
      , withI18n $ countVar 1 $ hauntedI "loseActions" a 2
      ]

instance RunMessage WitchHouseRuins where
  runMessage msg l@(WitchHouseRuins attrs) = runQueueT $ case msg of
    Msg.RevealLocation _ lid | lid == toId attrs -> do
      WitchHouseRuins <$> liftRunMessage msg (attrs & labelL .~ "witchHouseRuins")
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      investigateLocation_ sid iid (attrs.ability 1) attrs
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      loseActions iid (attrs.ability 2) 1
      pure l
    Successful (Action.Investigate, _) iid ab _ _ | isAbilitySource attrs 1 ab -> do
      healHorrorIfCan iid (attrs.ability 1) 2
      pure l
    _ -> WitchHouseRuins <$> liftRunMessage msg attrs
