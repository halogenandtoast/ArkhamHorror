-- | "__Revelation__ - Put Cryptic Message into play next to the agenda deck."
module Arkham.Homebrew.AndThenThereWereNone.Treacheries.CrypticMessage (crypticMessage) where

import Arkham.Ability
import Arkham.Helpers.Message.Discard.Lifted (chooseAndDiscardCards)
import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.AndThenThereWereNone.Helpers
import Arkham.Investigator.Types (Field (InvestigatorClues))
import Arkham.Matcher
import Arkham.Message.Lifted.Placement
import Arkham.Projection
import Arkham.Treachery.Import.Lifted

newtype CrypticMessage = CrypticMessage TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crypticMessage :: TreacheryCard CrypticMessage
crypticMessage = treachery CrypticMessage Cards.crypticMessage

{- | "__Forced__ - At the end of the round, each investigator chooses and discards
X cards from their hand, where X is the number of clues they control. Discard
Cryptic Message."
-}
instance HasAbilities CrypticMessage where
  getAbilities (CrypticMessage a) =
    [ scenarioI18n
        $ withI18nTooltip "crypticMessage.decipher"
        $ mkAbility a 1
        $ forced
        $ RoundEnds #when
    ]

instance RunMessage CrypticMessage where
  runMessage msg t@(CrypticMessage attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      place attrs NextToAgenda
      pure t
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      eachInvestigator \iid -> do
        n <- field InvestigatorClues iid
        chooseAndDiscardCards iid (attrs.ability 1) n
      toDiscard (attrs.ability 1) attrs
      pure t
    _ -> CrypticMessage <$> liftRunMessage msg attrs
