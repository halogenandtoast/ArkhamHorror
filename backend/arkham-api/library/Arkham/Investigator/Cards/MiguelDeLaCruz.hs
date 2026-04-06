module Arkham.Investigator.Cards.MiguelDeLaCruz (miguelDeLaCruz) where

import Arkham.Action.Additional
import Arkham.Helpers.Modifiers
import Arkham.I18n
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Trap, Trick))

newtype MiguelDeLaCruz = MiguelDeLaCruz InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

miguelDeLaCruz :: InvestigatorCard MiguelDeLaCruz
miguelDeLaCruz =
  investigator MiguelDeLaCruz Cards.miguelDeLaCruz
    $ Stats {health = 8, sanity = 8, willpower = 3, intellect = 2, combat = 3, agility = 4}

instance HasModifiersFor MiguelDeLaCruz where
  getModifiersFor (MiguelDeLaCruz a) =
    modifySelf
      a
      [ GiveAdditionalAction
          $ AdditionalAction "Miguel de la Cruz" (toSource a)
          $ PlayCardRestrictedAdditionalAction (basic #event)
      ]

instance HasChaosTokenValue MiguelDeLaCruz where
  getChaosTokenValue iid ElderSign (MiguelDeLaCruz attrs)
    | iid == attrs.id =
        pure $ ChaosTokenValue ElderSign (PositiveModifier 1)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage MiguelDeLaCruz where
  runMessage msg i@(MiguelDeLaCruz attrs) = runQueueT $ case msg of
    ElderSignEffect iid | iid == attrs.id -> do
      cards <-
        select
          $ inDiscardOf iid
          <> basic (mapOneOf CardWithTrait [Trick, Trap])
      unless (null cards) do
        chooseOneM iid do
          targets cards $ addToHand iid . only
          withI18n skip_
      pure i
    _ -> MiguelDeLaCruz <$> liftRunMessage msg attrs
