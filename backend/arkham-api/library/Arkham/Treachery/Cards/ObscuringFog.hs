module Arkham.Treachery.Cards.ObscuringFog (obscuringFog) where

import Arkham.Ability
import Arkham.Helpers.Location
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Matcher
import Arkham.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ObscuringFog = ObscuringFog TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

obscuringFog :: TreacheryCard ObscuringFog
obscuringFog = treachery ObscuringFog Cards.obscuringFog

instance HasModifiersFor ObscuringFog where
  getModifiersFor (ObscuringFog attrs) = case attrs.placement of
    AttachedToLocation lid -> modified_ attrs lid [ShroudModifier 2]
    _ -> pure mempty

instance HasAbilities ObscuringFog where
  getAbilities (ObscuringFog a) = case a.attached.location of
    Just lid ->
      [ forcedAbility a 1 $ SkillTestResult #after Anyone (WhileInvestigating (be lid)) #success
      ]
    _ -> []

instance RunMessage ObscuringFog where
  runMessage msg t@(ObscuringFog attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      withLocationOf iid \location -> do
        withoutObscuringFog <- selectNone $ treacheryAt location <> treacheryIs Cards.obscuringFog
        when withoutObscuringFog $ attachTreachery attrs location
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> ObscuringFog <$> liftRunMessage msg attrs
