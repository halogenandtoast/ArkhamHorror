module Arkham.Treachery.Cards.ObscuringFog where

import Arkham.Ability
import Arkham.Classes
import Arkham.Helpers.Investigator
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Placement
import Arkham.Prelude
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Helpers
import Arkham.Treachery.Runner

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
  getAbilities (ObscuringFog a) = case treacheryAttachedTarget a of
    Just (LocationTarget lid) ->
      [ forcedAbility a 1 $ SkillTestResult #after Anyone (WhileInvestigating $ LocationWithId lid) #success
      ]
    _ -> []

instance RunMessage ObscuringFog where
  runMessage msg t@(ObscuringFog attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      location <- getJustLocation iid
      withoutObscuringFog <- selectNone $ treacheryAt location <> treacheryIs Cards.obscuringFog
      pushWhen withoutObscuringFog $ attachTreachery attrs location
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> ObscuringFog <$> runMessage msg attrs
