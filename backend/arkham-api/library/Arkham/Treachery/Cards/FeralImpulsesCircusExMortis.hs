module Arkham.Treachery.Cards.FeralImpulsesCircusExMortis (feralImpulsesCircusExMortis) where

import Arkham.Ability
import Arkham.Campaigns.CircusExMortis.Helpers (getSealedMoonTokens)
import Arkham.Matcher
import Arkham.Message.Lifted.Placement (Placement (..), place)
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype FeralImpulses = FeralImpulses TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

feralImpulsesCircusExMortis :: TreacheryCard FeralImpulses
feralImpulsesCircusExMortis = treachery FeralImpulses Cards.feralImpulsesCircusExMortis

instance HasAbilities FeralImpulses where
  getAbilities (FeralImpulses a) =
    [ limited (MaxPer Cards.feralImpulsesCircusExMortis PerRound 1)
        $ mkAbility a 1
        $ forced
        $ PhaseBegins #when #mythos
    ]

instance RunMessage FeralImpulses where
  runMessage msg t@(FeralImpulses attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      place attrs NextToAgenda
      pure t
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      iids <- getInvestigators
      for_ iids \iid -> do
        tokens <- getSealedMoonTokens iid
        when (notNull tokens) $ chooseAndDiscardAsset iid attrs
      toDiscard (attrs.ability 1) attrs
      pure t
    _ -> FeralImpulses <$> liftRunMessage msg attrs
