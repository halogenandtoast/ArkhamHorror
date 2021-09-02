module Arkham.Types.Treachery.Cards.PsychopompsSong
  ( psychopompsSong
  , PsychopompsSong(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Exception
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype PsychopompsSong = PsychopompsSong TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

psychopompsSong :: TreacheryCard PsychopompsSong
psychopompsSong = treachery PsychopompsSong Cards.psychopompsSong

instance HasAbilities PsychopompsSong where
  getAbilities (PsychopompsSong attrs) = case treacheryAttachedTarget attrs of
    Just (InvestigatorTarget iid) ->
      [ mkAbility attrs 1
          $ ForcedAbility
          $ DealtDamage Timing.When
          $ InvestigatorWithId iid
      ]
    _ -> []

instance TreacheryRunner env => RunMessage env PsychopompsSong where
  runMessage msg t@(PsychopompsSong attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ push (AttachTreachery treacheryId $ InvestigatorTarget iid)
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      mMsg <- findFromQueue $ \case
        InvestigatorDamage iid' _ n _ | iid' == iid -> n > 0
        InvestigatorDoAssignDamage iid' _ _ n _ [] [] | iid' == iid -> n > 0
        _ -> False
      case mMsg of
        Just damageMsg -> do
          let
            newMsg = case damageMsg of
              InvestigatorDamage _ source' n horror ->
                InvestigatorDamage iid source' (n + 2) horror
              InvestigatorDoAssignDamage _ source' strategy n horror [] [] ->
                InvestigatorDoAssignDamage
                  iid
                  source'
                  strategy
                  (n + 2)
                  horror
                  []
                  []
              _ -> error "impossible"
          t <$ replaceMessage damageMsg [newMsg, Discard (toTarget attrs)]
        Nothing -> throwIO $ InvalidState "No damage occured"
    _ -> PsychopompsSong <$> runMessage msg attrs
