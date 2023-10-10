module Arkham.Treachery.Cards.PsychopompsSong (
  psychopompsSong,
  PsychopompsSong (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Helpers.Message
import Arkham.Helpers.Query
import Arkham.Matcher
import Arkham.Timing qualified as Timing
import Arkham.Treachery.Cards qualified as Cards
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
    Revelation iid source | isSource attrs source -> do
      investigators <- getInvestigators
      player <- getPlayer iid
      push
        $ chooseOrRunOne player
        $ [targetLabel iid' [AttachTreachery treacheryId $ InvestigatorTarget iid'] | iid' <- investigators]
      pure t
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      dealAdditionalDamage iid 2 [Discard (toAbilitySource attrs 1) (toTarget attrs)]
      pure t
    _ -> PsychopompsSong <$> runMessage msg attrs
