module Arkham.Treachery.Cards.PsychopompsSongUnionAndDisillusion (
  psychopompsSongUnionAndDisillusion,
  PsychopompsSongUnionAndDisillusion (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Helpers.Message
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype PsychopompsSongUnionAndDisillusion = PsychopompsSongUnionAndDisillusion TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

psychopompsSongUnionAndDisillusion :: TreacheryCard PsychopompsSongUnionAndDisillusion
psychopompsSongUnionAndDisillusion = treachery PsychopompsSongUnionAndDisillusion Cards.psychopompsSongUnionAndDisillusion

instance HasAbilities PsychopompsSongUnionAndDisillusion where
  getAbilities (PsychopompsSongUnionAndDisillusion attrs) = case treacheryAttachedTarget attrs of
    Just (InvestigatorTarget iid) ->
      [ mkAbility attrs 1 $
          ForcedAbility $
            DealtDamage Timing.When AnySource $
              InvestigatorWithId iid
      ]
    _ -> []

instance RunMessage PsychopompsSongUnionAndDisillusion where
  runMessage msg t@(PsychopompsSongUnionAndDisillusion attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source
      | isSource attrs source ->
          t <$ push (AttachTreachery treacheryId $ InvestigatorTarget iid)
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      dealAdditionalDamage iid 2 [Discard (toAbilitySource attrs 1) (toTarget attrs)]
      pure t
    _ -> PsychopompsSongUnionAndDisillusion <$> runMessage msg attrs
