module Arkham.Treachery.Cards.SordidAndSilent (
  SordidAndSilent (..),
  sordidAndSilent,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Timing qualified as Timing
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype SordidAndSilent = SordidAndSilent TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sordidAndSilent :: TreacheryCard SordidAndSilent
sordidAndSilent = treachery SordidAndSilent Cards.sordidAndSilent

instance HasAbilities SordidAndSilent where
  getAbilities (SordidAndSilent x) =
    [ mkAbility x 1 $ ForcedAbility $ RoundEnds Timing.When
    , mkAbility x 2 $ ForcedAbility $ AgendaAdvances Timing.When AnyAgenda
    ]

instance RunMessage SordidAndSilent where
  runMessage msg t@(SordidAndSilent attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      mlid <- field InvestigatorLocation iid
      for_ mlid $ \lid ->
        push $ AttachTreachery (toId attrs) $ LocationTarget lid
      pure t
    UseCardAbility _ source 1 _ _ | isSource attrs source ->
      case treacheryAttachedTarget attrs of
        Just (LocationTarget lid) -> do
          iids <- selectList $ InvestigatorAt $ LocationWithId lid
          pushAll
            [ InvestigatorAssignDamage iid source DamageAny 0 1
            | iid <- iids
            ]
          pure t
        _ -> pure t
    UseCardAbility _ source 2 _ _ | isSource attrs source -> do
      t <$ push (toDiscard (toAbilitySource attrs 2) attrs)
    _ -> SordidAndSilent <$> runMessage msg attrs
