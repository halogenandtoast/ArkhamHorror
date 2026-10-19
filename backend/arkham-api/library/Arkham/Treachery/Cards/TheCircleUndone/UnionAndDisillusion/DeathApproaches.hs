module Arkham.Treachery.Cards.TheCircleUndone.UnionAndDisillusion.DeathApproaches (deathApproaches) where

import Arkham.Ability
import Arkham.Helpers.Message qualified as Msg
import Arkham.Helpers.Query
import Arkham.Matcher
import Arkham.Treachery.CardDefs.TheCircleUndone.UnionAndDisillusion qualified as Cards
import Arkham.Treachery.Helpers qualified as Msg
import Arkham.Treachery.Import.Lifted

newtype DeathApproaches = DeathApproaches TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deathApproaches :: TreacheryCard DeathApproaches
deathApproaches = treachery DeathApproaches Cards.deathApproaches

instance HasAbilities DeathApproaches where
  getAbilities (DeathApproaches attrs) =
    [ restricted attrs 1 (InThreatAreaOf You)
        $ forced
        $ InvestigatorWouldTakeHorror #when You AnySource
    ]

instance RunMessage DeathApproaches where
  runMessage msg t@(DeathApproaches attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      investigators <- getInvestigators
      chooseOrRunOne iid $ targetLabels investigators $ only . Msg.placeInThreatArea attrs
      pure t
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      for_ attrs.inThreatAreaOf \iid ->
        dealAdditionalHorror iid 2 [Msg.toDiscardBy iid (attrs.ability 1) attrs]
      pure t
    _ -> DeathApproaches <$> liftRunMessage msg attrs
