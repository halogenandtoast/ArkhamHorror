module Arkham.Treachery.Cards.ObscuringFog where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Classes
import Arkham.Matcher
import Arkham.Message
import Arkham.Modifier
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Treachery.Attrs
import Arkham.Treachery.Helpers
import Arkham.Treachery.Runner

newtype ObscuringFog = ObscuringFog TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

obscuringFog :: TreacheryCard ObscuringFog
obscuringFog = treachery ObscuringFog Cards.obscuringFog

instance HasModifiersFor env ObscuringFog where
  getModifiersFor _ (LocationTarget lid) (ObscuringFog attrs) =
    pure
      $ toModifiers attrs [ ShroudModifier 2 | treacheryOnLocation lid attrs ]
  getModifiersFor _ _ _ = pure []

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

instance TreacheryRunner env => RunMessage env ObscuringFog where
  runMessage msg t@(ObscuringFog attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      currentLocationId <- getId iid
      obscuringFogCount <- selectCount $ TreacheryAt (LocationWithId currentLocationId) <> treacheryIs Cards.obscuringFog
      if obscuringFogCount > 0
        then pure t
        else do
          t <$ push
            (AttachTreachery treacheryId $ LocationTarget currentLocationId)
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      t <$ push (Discard $ toTarget attrs)
    _ -> ObscuringFog <$> runMessage msg attrs
