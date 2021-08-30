module Arkham.Types.Treachery.Cards.SordidAndSilent
  ( SordidAndSilent(..)
  , sordidAndSilent
  ) where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype SordidAndSilent = SordidAndSilent TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sordidAndSilent :: TreacheryCard SordidAndSilent
sordidAndSilent = treachery SordidAndSilent Cards.sordidAndSilent

instance HasAbilities env SordidAndSilent where
  getAbilities _ _ (SordidAndSilent x) = pure
    [ mkAbility x 1 $ ForcedAbility $ RoundEnds Timing.When
    , mkAbility x 2 $ ForcedAbility $ AgendaAdvances Timing.When AnyAgenda
    ]

instance TreacheryRunner env => RunMessage env SordidAndSilent where
  runMessage msg t@(SordidAndSilent attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      lid <- getId @LocationId iid
      t <$ push (AttachTreachery (toId attrs) $ LocationTarget lid)
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      case treacheryAttachedTarget attrs of
        Just (LocationTarget lid) -> do
          iids <- selectList $ InvestigatorAt $ LocationWithId lid
          t
            <$ pushAll
                 [ InvestigatorAssignDamage iid source DamageAny 0 1
                 | iid <- iids
                 ]
        _ -> pure t
    UseCardAbility _ source _ 2 _ | isSource attrs source ->
      t <$ push (Discard $ toTarget attrs)
    _ -> SordidAndSilent <$> runMessage msg attrs
