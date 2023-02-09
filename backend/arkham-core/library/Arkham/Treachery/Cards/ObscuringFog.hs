module Arkham.Treachery.Cards.ObscuringFog where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Helpers.Investigator
import Arkham.Matcher
import Arkham.Message
import Arkham.Modifier
import Arkham.Target
import qualified Arkham.Timing as Timing
import qualified Arkham.Treachery.Cards as Cards
import Arkham.Treachery.Helpers
import Arkham.Treachery.Runner

newtype ObscuringFog = ObscuringFog TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

obscuringFog :: TreacheryCard ObscuringFog
obscuringFog = treachery ObscuringFog Cards.obscuringFog

instance HasModifiersFor ObscuringFog where
  getModifiersFor (LocationTarget lid) (ObscuringFog attrs) =
    pure
      $ toModifiers attrs [ ShroudModifier 2 | treacheryOnLocation lid attrs ]
  getModifiersFor _ _ = pure []

instance HasAbilities ObscuringFog where
  getAbilities (ObscuringFog a) = case treacheryAttachedTarget a of
    Just (LocationTarget lid) ->
      [ mkAbility a 1 $ ForcedAbility $ SkillTestResult
          Timing.After
          Anyone
          (WhileInvestigating $ LocationWithId lid)
          (SuccessResult AnyValue)
      ]
    _ -> []

instance RunMessage ObscuringFog where
  runMessage msg t@(ObscuringFog attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      currentLocationId <- getJustLocation iid
      withoutObscuringFog <-
        selectNone
        $ TreacheryAt (LocationWithId currentLocationId)
        <> treacheryIs Cards.obscuringFog
      when withoutObscuringFog
        $ push
        $ AttachTreachery treacheryId
        $ LocationTarget currentLocationId
      pure t
    UseCardAbility _ source 1 _ _ | isSource attrs source ->
      t <$ push (Discard (toAbilitySource attrs 1) $ toTarget attrs)
    _ -> ObscuringFog <$> runMessage msg attrs
