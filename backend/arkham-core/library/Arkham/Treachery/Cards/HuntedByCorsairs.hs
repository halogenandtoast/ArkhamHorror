module Arkham.Treachery.Cards.HuntedByCorsairs (huntedByCorsairs, HuntedByCorsairs (..)) where

import Arkham.Ability
import Arkham.Classes
import Arkham.Helpers.Act
import Arkham.Matcher
import Arkham.Message
import Arkham.Prelude
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype HuntedByCorsairs = HuntedByCorsairs TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

huntedByCorsairs :: TreacheryCard HuntedByCorsairs
huntedByCorsairs = treachery HuntedByCorsairs Cards.huntedByCorsairs

instance HasAbilities HuntedByCorsairs where
  getAbilities (HuntedByCorsairs attrs) = case attrs.attached of
    Just (ActTarget aid) ->
      [ mkAbility attrs 1 $ ForcedAbility $ ActAdvances #when (ActWithId aid)
      , mkAbility (proxy (ActWithId aid) attrs) 2 actionAbility
      ]
    _ -> []

instance RunMessage HuntedByCorsairs where
  runMessage msg t@(HuntedByCorsairs attrs) = case msg of
    Revelation _iid (isSource attrs -> True) -> do
      currentAct <- getCurrentAct
      push $ attachTreachery attrs currentAct
      pure t
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      investigators <- getInvestigators
      pushAll [assignDamage iid (attrs.ability 1) 2 | iid <- investigators]
      pure t
    UseThisAbility iid source@(isProxySource attrs -> True) 2 -> do
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [SkillLabel sType [beginSkillTest iid source iid sType 4] | sType <- [#intellect, #agility]]
      pure t
    PassedThisSkillTest iid source@(isProxySource attrs -> True) -> do
      push $ toDiscardBy iid source attrs
      pure t
    _ -> HuntedByCorsairs <$> runMessage msg attrs
