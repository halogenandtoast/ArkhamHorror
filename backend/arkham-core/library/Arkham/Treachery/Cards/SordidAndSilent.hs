module Arkham.Treachery.Cards.SordidAndSilent
  ( SordidAndSilent(..)
  , sordidAndSilent
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Classes
import Arkham.Id
import Arkham.Matcher
import Arkham.Message
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Treachery.Attrs
import Arkham.Treachery.Runner

newtype SordidAndSilent = SordidAndSilent TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sordidAndSilent :: TreacheryCard SordidAndSilent
sordidAndSilent = treachery SordidAndSilent Cards.sordidAndSilent

instance HasAbilities SordidAndSilent where
  getAbilities (SordidAndSilent x) =
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
