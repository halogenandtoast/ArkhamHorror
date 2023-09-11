module Arkham.Treachery.Cards.ObscuringFog where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Helpers.Investigator
import Arkham.Matcher
import Arkham.Message
import Arkham.Modifier
import Arkham.Timing qualified as Timing
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Helpers
import Arkham.Treachery.Runner

newtype ObscuringFog = ObscuringFog TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

obscuringFog :: TreacheryCard ObscuringFog
obscuringFog = treachery ObscuringFog Cards.obscuringFog

instance HasModifiersFor ObscuringFog where
  getModifiersFor (LocationTarget lid) (ObscuringFog attrs) =
    pure $ toModifiers attrs [ShroudModifier 2 | treacheryOnLocation lid attrs]
  getModifiersFor _ _ = pure []

instance HasAbilities ObscuringFog where
  getAbilities (ObscuringFog a) = case treacheryAttachedTarget a of
    Just (LocationTarget lid) ->
      [ forcedAbility a 1
          $ SkillTestResult
            Timing.After
            Anyone
            (WhileInvestigating $ LocationWithId lid)
            (SuccessResult AnyValue)
      ]
    _ -> []

instance RunMessage ObscuringFog where
  runMessage msg t@(ObscuringFog attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      location <- getJustLocation iid
      withoutObscuringFog <- selectNone $ treacheryAt location <> treacheryIs Cards.obscuringFog
      pushWhen withoutObscuringFog $ AttachTreachery treacheryId $ LocationTarget location
      pure t
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      push $ Discard (toAbilitySource attrs 1) (toTarget attrs)
      pure t
    _ -> ObscuringFog <$> runMessage msg attrs
