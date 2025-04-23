module Arkham.Treachery.Cards.OppressiveMists (oppressiveMists) where

import Arkham.Ability
import Arkham.Helpers.Location
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Matcher
import Arkham.Helpers.Window (cardsDrawn)
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype OppressiveMists = OppressiveMists TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oppressiveMists :: TreacheryCard OppressiveMists
oppressiveMists = treachery OppressiveMists Cards.oppressiveMists

instance HasAbilities OppressiveMists where
  getAbilities (OppressiveMists a) = case a.attached.location of
    Just lid ->
      [ forcedAbility a 1 $ DrawsCards #after (You <> at_ (be lid)) AnyValue
      , restricted a 2 OnSameLocation doubleActionAbility
      ]
    _ -> []

instance RunMessage OppressiveMists where
  runMessage msg t@(OppressiveMists attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      withLocationOf iid \location -> do
        ok <- selectNone $ treacheryAt location <> treacheryIs Cards.oppressiveMists
        when ok $ attachTreachery attrs location
      pure t
    UseCardAbility iid (isSource attrs -> True) 1 (cardsDrawn -> drawn) _ -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 3)
      pure $ OppressiveMists $ attrs & metaL .~ toJSON (length drawn)
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      let n = toResultDefault 0 attrs.meta
      chooseAndDiscardCards iid (attrs.ability 1) n
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> OppressiveMists <$> liftRunMessage msg attrs
