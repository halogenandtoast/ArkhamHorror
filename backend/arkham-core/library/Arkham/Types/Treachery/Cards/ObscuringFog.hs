module Arkham.Types.Treachery.Cards.ObscuringFog where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Card.CardCode
import Arkham.Types.Classes
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.Target
import Arkham.Types.Timing qualified as Timing
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Helpers
import Arkham.Types.Treachery.Runner

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
      obscuringFogCount <- unTreacheryCount
        <$> getCount (currentLocationId, toCardCode attrs)
      if obscuringFogCount > 0
        then pure t
        else do
          t <$ push
            (AttachTreachery treacheryId $ LocationTarget currentLocationId)
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      t <$ push (Discard $ toTarget attrs)
    _ -> ObscuringFog <$> runMessage msg attrs
