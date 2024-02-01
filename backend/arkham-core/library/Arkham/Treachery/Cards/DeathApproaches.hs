module Arkham.Treachery.Cards.DeathApproaches (
  deathApproaches,
  DeathApproaches (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Helpers.Message
import Arkham.Helpers.Query
import Arkham.Matcher
import Arkham.Timing qualified as Timing
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype DeathApproaches = DeathApproaches TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

deathApproaches :: TreacheryCard DeathApproaches
deathApproaches = treachery DeathApproaches Cards.deathApproaches

instance HasAbilities DeathApproaches where
  getAbilities (DeathApproaches attrs) = case treacheryAttachedTarget attrs of
    Just (InvestigatorTarget iid) ->
      [ mkAbility attrs 1
          $ ForcedAbility
          $ DealtHorror Timing.When AnySource
          $ InvestigatorWithId iid
      ]
    _ -> []

instance RunMessage DeathApproaches where
  runMessage msg t@(DeathApproaches attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      investigators <- getInvestigators
      player <- getPlayer iid
      push
        $ chooseOrRunOne player
        $ [targetLabel iid' [AttachTreachery treacheryId $ InvestigatorTarget iid'] | iid' <- investigators]
      pure t
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      dealAdditionalHorror iid 2 [toDiscardBy iid (toAbilitySource attrs 1) attrs]
      pure t
    _ -> DeathApproaches <$> runMessage msg attrs
