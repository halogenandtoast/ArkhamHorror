module Arkham.Treachery.Cards.PsychopompsSong
  ( psychopompsSong
  , PsychopompsSong(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Classes
import Arkham.Exception
import Arkham.Matcher
import Arkham.Message
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Treachery.Runner

newtype PsychopompsSong = PsychopompsSong TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

psychopompsSong :: TreacheryCard PsychopompsSong
psychopompsSong = treachery PsychopompsSong Cards.psychopompsSong

instance HasAbilities PsychopompsSong where
  getAbilities (PsychopompsSong attrs) = case treacheryAttachedTarget attrs of
    Just (InvestigatorTarget iid) ->
      [ mkAbility attrs 1
          $ ForcedAbility
          $ DealtDamage Timing.When AnySource
          $ InvestigatorWithId iid
      ]
    _ -> []

instance RunMessage PsychopompsSong where
  runMessage msg t@(PsychopompsSong attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ push (AttachTreachery treacheryId $ InvestigatorTarget iid)
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      mMsg <- findFromQueue $ \case
        InvestigatorDamage iid' _ n _ | iid' == iid -> n > 0
        InvestigatorDoAssignDamage iid' _ _ _ n _ [] [] | iid' == iid -> n > 0
        _ -> False
      case mMsg of
        Just damageMsg -> do
          let
            newMsg = case damageMsg of
              InvestigatorDamage _ source' n horror ->
                InvestigatorDamage iid source' (n + 2) horror
              InvestigatorDoAssignDamage _ source' strategy matcher n horror [] [] ->
                InvestigatorDoAssignDamage
                  iid
                  source'
                  strategy
                  matcher
                  (n + 2)
                  horror
                  []
                  []
              _ -> error "impossible"
          t <$ replaceMessage damageMsg [newMsg, Discard (toAbilitySource attrs 1) (toTarget attrs)]
        Nothing -> throwIO $ InvalidState "No damage occured"
    _ -> PsychopompsSong <$> runMessage msg attrs
